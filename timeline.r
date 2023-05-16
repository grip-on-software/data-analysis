# Script that collects data for a timeline visualization.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
# Copyright 2017-2023 Leon Helwerda
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

library(jsonlite)
library(logging)
library(plyr)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/features.r')
source('include/sources.r')
source('include/project.r')

date_format <- function(date) {
    format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

make_opt_parser(desc="Collect data for timeline visualization",
                options=list(make_option('--project-ids', default='0',
                                         help='Anonymize projects (0 or 1)'),
                             make_option('--sprint-ids', default='0',
                                         help='Anonymize sprints (0 or 1)'),
                             make_option('--output', default='output',
                                         help='Output directory'),
                             make_option('--days', default=NA_integer_,
                                         help=paste('Number of days before a',
                                                    'sprint is left out')),
                             make_option('--patch', action='store_true',
                                         default=FALSE,
                                         help='Exclude patch sprints'),
                             make_option('--latest-date',
                                         default=as.character(Sys.time()),
                                         help=paste('Sprint start date/time',
                                                    'after which later sprints',
                                                    'are left out')),
                             make_option('--features', default=NA_character_,
                                         help='List of features to retrieve'),
                             make_option('--exclude', default='^$',
                                         help=paste('Regular expression of',
                                                    'features to filter')),
                             make_option('--events', default='',
                                         help='List of events to retrieve'),
                             make_option('--no-features', dest='no_features',
                                         action='store_true', default=F,
                                         help=paste('Do not filter events on',
                                                    'sprints with features,',
                                                    'e.g., when no features',
                                                    'were retrieved'))),
                variables=get_config_fields())
config <- get_config()
arguments <- config$args
log_setup(arguments)
conn <- connect()

if (config$db$primary_source == "tfs") {
    join_cols <- c("team_id", "sprint_id")
    project_fields <- list(project_id='team_id', name='${s(project_name)}')
} else {
    join_cols <- c("project_id", "sprint_id")
    project_fields <- list(project_id='project_id', name='${s(project_name)}')
}

project_ids <- arguments$project_ids
if (project_ids != '0') {
    project_ids <- '1'
}
sprint_ids <- arguments$sprint_ids
if (sprint_ids != '0') {
    sprint_ids <- '1'
}

variables <- c(config$fields,
               list(project_ids=project_ids, sprint_ids=sprint_ids))
patterns <- load_definitions('sprint_definitions.yml', variables)

projects <- get_projects_meta(conn, fields=project_fields,
                              metadata=list(core=T), join_cols=join_cols,
                              patterns=patterns)
projects <- projects[projects$core, ]

output_directory <- arguments$output

items <- load_queries('sprint_events.yml',
                      variables=c(list(join_cols=join_cols), patterns))

export_features <- function(features, exclude, output_directory) {
    result <- get_sprint_features(conn, features, exclude, variables,
                                  latest_date=as.POSIXct(arguments$latest_date),
                                  sprint_days=arguments$days,
                                  sprint_patch=ifelse(arguments$patch, NA, F),
                                  future=F)
    data <- result$data
    project_col <- result$join_cols[1]
    sprint_col <- result$join_cols[2]
    colnames <- result$colnames[result$colnames != project_col &
                                result$colnames != sprint_col]
    project_data <- lapply(as.list(projects$project_id), function(project) {
        project_id <- projects[project, 'project_id']
        if (project_id %in% data[[project_col]]) {
            sprint_data <- data[data[[project_col]] == project,
                                c(sprint_col, colnames)]
            result <- lapply(as.list(seq_len(dim(sprint_data)[1])),
                             function(i) {
                                 safe_unbox(sprint_data[i, colnames])
                             })
            names(result) <- sprint_data[[sprint_col]]
            return(result)
        }
        return(NA)
    })
    names(project_data) <- projects$name
    write(toJSON(get_feature_locales(result$items)),
          file=paste(output_directory, "locales.json", sep="/"))
    write(toJSON(project_data),
          file=paste(output_directory, "features.json", sep="/"))
    return(data)
}

# Export data to separate per-sprint files.
export_split_data <- function(data, item, output_directory) {
    ids <- as.list(seq_len(dim(projects)[1]))
    project_data <- lapply(ids, function(project) {
        project_id <- projects[project, 'project_id']
        if (project_ids != '1') {
            project_name <- projects[project, 'name']
        } else {
            project_name <- paste('Proj', project_id, sep='')
        }

        query <- paste('SELECT ${f(join_cols, "sprint", mask=2)}
                        FROM gros.${t("sprint")}
                        WHERE ${f(join_cols, "sprint", mask=1)} =',
                        project_id)
        sprint_item <- load_query(list(query=query),
                                  c(patterns, list(join_cols=join_cols)))
        logdebug(sprint_item$query)
        sprints <- dbGetQuery(conn, sprint_item$query)
        if (nrow(sprints) == 0) {
            return(sprints)
        }

        path <- paste(output_directory, project_name, sep="/")
        if (!dir.exists(path)) {
            dir.create(path)
        }

        sprint_col <- join_cols[2]
        for (sprint_id in sprints[[sprint_col]]) {
            sprint_split_data <- data[data$project_name == project_name &
                                      data[[sprint_col]] == sprint_id, ]

            filename <- paste(path,
                              paste(item$type, sprint_id, "json", sep="."),
                              sep="/")
            write(toJSON(sprint_split_data), file=filename)
        }
        return(sprints)
    })
    names(project_data) <- projects$name
    return(project_data)
}

# Export result of a type query to the correct JSON file(s).
export_data <- function(data, item, output_directory) {
    if (isTRUE(item$split)) {
        return(export_split_data(data, item, output_directory))
    }
    project_names <- as.list(projects$name)
    if ('board_id' %in% colnames(data)) {
        project_boards <- lapply(project_names, function(project) {
            result <- data[data$project_name == project, ]
            board_id <- unique(result[!is.na(result$board_id), 'board_id'])
            if (length(board_id) == 1) {
                return(safe_unbox(board_id))
            }
            return(safe_unbox(NA))
        })

        names(project_boards) <- projects$name
        write(toJSON(project_boards),
              file=paste(output_directory, "boards.json", sep="/"))
    }
    data[!is.na(project_boards[data$project_name]), 'board_id'] <- NA
    project_data <- lapply(project_names, function(project) {
        return(data[data$project_name == project, ])
    })
    names(project_data) <- projects$name
    path <- paste(output_directory, paste(item$type, "json", sep="."), sep="/")
    write(toJSON(project_data), file=path)

    return(project_data)
}

# Perform all queries and extract the extrema data.
min_date <- list()
max_date <- list()
types <- list()
projects_with_data <- list()
project_boards <- list()

events <- strsplit(arguments$events, ',')[[1]]
data <- export_features(arguments$features, arguments$exclude, output_directory)
sprint_col <- join_cols[2]
for (item in items) {
    if (length(events) > 0 && !(item$type %in% events)) {
        next
    }
    loginfo('Executing query for type %s', item$type)
    logdebug(item$query)
    time <- system.time(result <- dbGetQuery(conn, item$query))
    loginfo('Query for type %s took %f seconds', item$type, time['elapsed'])
    if (!arguments$no_features) {
        result <- result[result[[sprint_col]] %in% data[[sprint_col]], ]
    }
    if (nrow(result) > 0) {
        result$date <- date_format(result$date)
        result$type <- item$type
        if ("end_date" %in% colnames(result)) {
            result$end_date <- date_format(result$end_date)
        }
        project_data <- export_data(result, item, output_directory)
        have_data <- lapply(project_data, nrow) > 0
        projects_with_data <- modifyList(projects_with_data,
                                          as.list(have_data)[have_data])

        min_date[[item$type]] <- min(result$date, na.rm=T)
        max_date[[item$type]] <- max(result$date, result$end_date, na.rm=T)

        type <- list(name=safe_unbox(item$type),
                      locales=safe_unbox(item$descriptions))
        if (!is.null(item$display)) {
            type$enabled <- safe_unbox(item$display)
        }
        if (!is.null(item$split)) {
            type$subchart <- safe_unbox(item$split)
        }
        types <- c(types, list(type))
    } else {
        loginfo(paste('No matching data for', item$type, 'event'))
    }
}

total_data <- list(min_date=safe_unbox(min(unlist(min_date), na.rm=T)),
                   max_date=safe_unbox(max(unlist(max_date), na.rm=T)),
                   update_date=safe_unbox(date_format(arguments$latest_date)),
                   projects=names(projects_with_data),
                   boards=project_boards)

write(toJSON(total_data), file=paste(output_directory, "data.json", sep="/"))
write(toJSON(types), file=paste(output_directory, "types.json", sep="/"))

if (project_ids != '1') {
    ids <- as.list(projects[projects$name %in% names(projects_with_data),
                            'project_id'])
    projects_with_data <- modifyList(projects_with_data, ids)

    links <- mapply(function(project_id) {
                        safe_unbox(get_source_urls(conn, project_id, c('jira')))
                    },
                    projects_with_data, SIMPLIFY=F)

    write(toJSON(links), file=paste(output_directory, "links.json", sep="/"))
} else {
    write("{}", file=paste(output_directory, "links.json", sep="/"))
}
