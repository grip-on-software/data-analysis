# Script to generate CSV file containing a measured backlog size between certain
# intervals defined by project length.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source('include/args.r')
source('include/log.r')
source('include/project.r')

make_opt_parser(desc="Generate CSV of backlog sizes at project-based intervals",
                options=list(make_option('--output', default='output',
                                         help='Output directory'),
                             make_option('--count', default=4,
                                         help='Number of intervals')),
                variables=get_config_fields())
config <- get_config()
arguments <- config$args
log_setup(arguments)

conn <- connect()
patterns <- load_definitions('sprint_definitions.yml', config$fields)
output_directory <- arguments$output
count <- arguments$count

fields <- c('project_id', 'name', 'quality_display_name')
projects <- get_projects_meta(conn,
                              fields=fields,
                              metadata=list(core=T, main=T))
intervals <- get_project_intervals(conn, count)
projects <- projects[projects$core & projects$main &
                     projects$project_id %in% colnames(intervals), ]

project_backlog_size <- function(project_id, name, quality_name) {
    if (!is.na(quality_name)) {
        name <- quality_name
    }
    loginfo(paste("Calculating values at intervals for", name))
    interval <- c(0, intervals[, as.character(project_id)])
    baclog_size <- function(index) {
        domain <- c(interval[index-1], interval[index])
        constraint <- paste('CAST(', as.Date(domain, origin="1970-01-01"),
                            ' AS TIMESTAMP)', sep='\'', collapse=' AND ')
        query <- paste('SELECT COUNT(DISTINCT issue.issue_id) AS backlog_size
                        FROM gros.issue
                        WHERE issue.project_id =', project_id,
                        'AND issue.updated BETWEEN', constraint,
                        'AND ${s(issue_not_done)}')
        item <- load_query(list(query=query), patterns)
        dbGetQuery(conn, item$query)$backlog_size
    }
    sizes <- lapply(seq(2, length(interval)), backlog_size)
    return(c(name, sizes))
}

out <- mapply(project_backlog_size, projects$project_id, projects$name,
              projects$quality_display_name, SIMPLIFY=T)

write.table(t(out), sep=",",
            file=paste(output_directory, 'backlog_size_interval.csv', sep='/'),
            row.names=F, col.names=c("Project", paste("Interval", 1:count)))
