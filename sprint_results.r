# Script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.
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

library(foreign)
library(jsonlite)
library(yaml)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/log.r')
source('include/project.r')
source('include/sources.r')
source('include/tracker.r')

make_opt_parser(desc="Combine prediction run output with sprint data for API",
                options=list(make_option('--file', default='sprint_labels.json',
                                         help='Prediction run result input'),
                             make_option('--features',
                                         default='output/sprint_features.arff',
                                         help='Features data set input file'),
                             make_option('--output', default='output',
                                         help='Output directory'),
                             make_option('--project-ids', default='0',
                                         help='Anonymize projects (0 or 1)'),
                             make_option('--sprint-ids', default='0',
                                         help='Anonymize sprints (0 or 1)'),
                             make_option('--project-metadata',
                                         default='recent,core,main',
                                         help=paste('List of project metadata',
                                                    'fields to output')),
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
                             make_option('--recent-date',
                                         default=as.character(Sys.Date() -
                                             as.difftime(12, units="weeks")),
                                         help=paste('Date from which projects',
                                                    'should have sprints to be',
                                                    'considered recent')),
                             make_option('--core', action='store_true',
                                         default=FALSE,
                                         help=paste('Only consider non-support',
                                                    'team, main projects'))),
                variables=get_config_fields())
config <- get_config()
arguments <- config$args
log_setup(arguments)

latest_date <- as.POSIXct(arguments$latest_date)
sprint_days <- arguments$days
sprint_patch <- ifelse(arguments$patch, NA, F)
core <- arguments$core

projects <- list()
specifications <- yaml.load_file('sprint_features.yml')
specifications$files <- c(specifications$files, list(get_time_feature()))

join_cols <- c('project_id')
if (config$db$primary_source == "tfs") {
    join_cols <- c('team_id')
}

output_directory <- arguments$output
project_ids <- arguments$project_ids
if (project_ids != '0') {
    project_ids <- '1'
}
sprint_ids <- arguments$sprint_ids
if (sprint_ids != '0') {
    sprint_ids <- '1'
}

metadata <- get_meta_keys(arguments$project_metadata, arguments$recent_date)
fields <- list(join_cols, 'name')
if (config$db$primary_source != "tfs") {
    fields <- c(fields, 'quality_display_name')
}

patterns <- load_definitions('sprint_definitions.yml',
                             c(config$fields, list(join_cols=join_cols)),
                             current_time=latest_date)

get_sprints <- function(conn) {
    conditions <- get_sprint_conditions(latest_date=latest_date, core=core,
                                        sprint_days=sprint_days,
                                        sprint_patch=sprint_patch)
    # Fields are anonymized for project_ids/sprint_ids later in anonymize_result
    fields <- list(project_id='${t("project")}.project_id',
                   project_key='${t("project")}.name',
                   quality_name='${t("project")}.quality_name',
                   quality_display_name='${t("project")}.quality_display_name',
                   sprint_id='${t("sprint")}.sprint_id',
                   name='${t("sprint")}.name',
                   start_date='${t("sprint")}.start_date',
                   close_date='${s(sprint_close)}',
                   board_id='${t("sprint")}.board_id')
    if (config$db$primary_source == "tfs") {
        fields$project_id <- '${t("project")}.team_id'
        fields$quality_name <- NULL
        fields$quality_display_name <- NULL
        fields$board_id <- NULL
    } else if (config$db$primary_source == "jira_version") {
        fields$sprint_id <- '${t("sprint")}.sprint_id'
        fields$board_id <- NULL
    }
    query <- paste('SELECT', paste(format_aliases(fields), collapse=", "), '
                    FROM gros.${t("sprint")}
                    JOIN gros.${t("project")}
                    ON ${j(join_cols, "project", "sprint", mask=1)}
                    WHERE', paste(conditions, collapse=' AND '), '
                    ORDER BY ${f(join_cols, "project", mask=1)},
                    ${s(sprint_open)}, ${t("sprint")}.name')
    variables <- c(patterns, list(join_cols=join_cols,
                                  sprint_days=sprint_days))
    item <- load_query(list(query=query), variables)
    logdebug(item$query)
    time <- system.time(sprint <- dbGetQuery(conn, item$query))
    loginfo('Obtained sprints in %f seconds', time['elapsed'])
    return(sprint)
}

conn <- connect()
sprint_cache <- get_sprints(conn)
get_sprint <- function(project_id, sprint_num) {
    return(sprint_cache[sprint_cache$project_id == project_id, ][sprint_num, ])
}
get_sprint_by_id <- function(project_id, sprint_id) {
    return(sprint_cache[sprint_cache$project_id == project_id &
                        sprint_cache$sprint_id == sprint_id, ])
}
get_project <- function(project_id) {
    return(sprint_cache[sprint_cache$project_id == project_id, ])
}

# Read prediction results input file
results <- read_json(arguments$file, simplifyVector=T)
if (is.list(results$projects)) {
    logwarn("Results is a multiple-run output which we cannot parse.")
    quit("no", status=0, runLast=F)
}
# Read data set of features
features <- read.arff(arguments$features)

get_tags <- function(features_row) {
    tags <- list()
    for (file in specifications$files) {
        if (!is.null(file$tags) && file$column %in% colnames(features_row)) {
            tags <- c(tags,
                      file$column[as.logical(features_row[[file$column]])])
        }
    }
    return(tags)
}

anonymize_result <- function(sprint, project_id) {
    if (project_ids != '0') {
        sprint$project <- paste("Proj", project_id, sep="")
        sprint$project_id <- sprint$project
    }
    if (sprint_ids != '0') {
        sprint$name <- paste("Sprint", sprint$sprint)
    }
    return(sprint)
}

get_analogy_results <- function(i, idx) {
    analogy <- results$analogy_indexes[idx, i]
    analogy_sprint <- get_sprint(features[analogy, "project_id"],
                                 features[analogy, "sprint_num"])
    analogy_values <- as.list(results$analogy_values[idx, i, ])

    names(analogy_values) <- feature_names
    analogy_value <- modifyList(analogy_values,
                                as.list(features[analogy,
                                                 feature_mask]),
                                keep.null=T)
    result <- list(project=analogy_sprint$quality_display_name,
                   project_id=analogy_sprint$project_key,
                   sprint=features[analogy, "sprint_num"],
                   board_id=analogy_sprint$board_id,
                   id=analogy_sprint$sprint_id,
                   name=analogy_sprint$name,
                   start_date=as.POSIXct(analogy_sprint$start_date),
                   end_date=as.POSIXct(analogy_sprint$close_date),
                   label=results$analogy_labels[idx, i],
                   features=safe_unbox(analogy_value),
                   tags=get_tags(features[analogy, ]))
    return(anonymize_result(result, features[analogy, "project_id"]))
}

if (!is.null(results$configuration$assignments)) {
    assignments <- modifyList(results$configuration$assignments,
                              get_expressions_metadata(specifications$files,
                                                       features))
    results$configuration$assignments <- assignments
}
labels <- results$configuration$labels
results$configuration$label <- list(expression=results$configuration$label,
                                    attributes=as.list(labels))
results$configuration$labels <- NULL
results$configuration$organizations <- as.list(unique(results$organizations))

sprint_data <- get_sprint_projects(conn, patterns=patterns,
                                   join_cols=join_cols)
project_col <- join_cols[1]

if (!is.null(organization)) {
    organization_path <- paste(output_directory, organization, sep="/")
    if (!dir.exists(organization_path)) {
        dir.create(organization_path)
    }
} else {
    organization_path <- output_directory
}

tag_names <- get_tags(data.frame(as.list(setNames(rep(T, length(features)),
                                                  colnames(features)))))
feature_excludes <- c("project_id", "sprint_id", "organization", tag_names)
feature_mask <- !(names(features) %in% feature_excludes)
feature_names <- as.character(results$configuration$features)
feature_attributes <- unlist(unique(do.call(c, lapply(assignments,
                                                      function(assignment) {
                                                          assignment$attributes
                                                      }))))

for (idx in seq_along(results$projects)) {
    if (results$organizations[idx] != organization) {
        next
    }

    project_id <- results$projects[idx]
    sprint_id <- results$sprints[idx]
    sprint <- get_sprint_by_id(project_id, sprint_id)
    ids <- sort(results$sprints[results$organizations == organization &
                                results$projects == project_id])

    if (!is.null(results$analogy_indexes) &&
        nrow(results$analogy_indexes) >= idx) {
        analogies <- mapply(get_analogy_results,
                            seq_along(results$analogy_indexes[idx, ]),
                            MoreArgs=list(idx=idx), SIMPLIFY=F)
    } else {
        analogies <- NULL
    }

    if (project_ids != '1') {
        project_name <- sprint_data[sprint_data[[project_col]] == project_id,
                                    'name']
    } else {
        project_name <- paste("Proj", project_id, sep="")
    }
    projects <- c(projects, project_name)

    sprint_features <- as.list(results$features[idx, ])
    names(sprint_features) <- feature_names
    features_row <- features[features$project_id == project_id &
                             features$sprint_id == sprint_id, ]
    all_features <- modifyList(sprint_features,
                               as.list(features_row[, feature_mask]),
                               keep.null=T)
    result <- list(project=sprint$quality_display_name,
                   project_id=sprint$project_key,
                   sprint=all_features$sprint_num,
                   id=sprint$sprint_id,
                   board_id=sprint$board_id,
                   name=sprint$name,
                   start_date=as.POSIXct(sprint$start_date),
                   end_date=as.POSIXct(sprint$close_date),
                   prediction=results$labels[idx],
                   probability=results$probabilities[idx],
                   risk=results$risks[idx],
                   metrics=results$metrics,
                   analogies=analogies,
                   features=safe_unbox(all_features),
                   tags=get_tags(features_row),
                   configuration=results$configuration,
                   sources=get_tracker_dates(conn, project_id, aggregate=max))
    project_data <- anonymize_result(result, project_id)

    path <- paste(organization_path, project_name, sep="/")
    if (!dir.exists(path)) {
        dir.create(path)
    }
    data <- toJSON(project_data, auto_unbox=T, na="null", null="null")
    if (all(sprint_id <= ids)) {
        write(data, file=paste(path, "latest.json", sep="/"))
        project_sprints <- get_project(project_id)
        project_sprints$sprint_num <- row(project_sprints)[, 1]
        if (nrow(project_sprints) > 0) {
            if (sprint_ids != '0') {
                project_sprints$name <- paste("Sprint",
                                              project_sprints$sprint_num)
            }
        }
        write(toJSON(project_sprints[project_sprints$sprint_id %in% ids,
                                     c('name', 'sprint_num', 'sprint_id')]),
              file=paste(path, "sprints.json", sep="/"))
    }
    write(data, file=paste(path, paste(sprint_id, "json", sep="."), sep="/"))

    sprint_links <- paste("links", sprint_id, "json", sep=".")
    if (project_ids != '1') {
        source_urls <- get_source_urls(conn, project_id)
        source_patterns <- list(quality_name=sprint$quality_name)
        write(toJSON(build_project_source_urls(source_urls, project_id,
                                               project_name, source_patterns),
                     auto_unbox=T), file=paste(path, "sources.json", sep="/"))

        source_data <- toJSON(build_sprint_source_urls(source_urls, project_id,
                                                       project_name,
                                                       sprint$quality_name,
                                                       sprint,
                                                       specifications$files,
                                                       patterns))
        write(source_data, file=paste(path, sprint_links, sep="/"))
        if (all(sprint_id <= ids)) {
            write(source_data, file=paste(path, "links.json", sep="/"))
        }
    } else {
        write("{}", file=paste(path, "sources.json", sep="/"))
        write("{}", file=paste(path, sprint_links, sep="/"))
        write("{}", file=paste(path, "links.json", sep="/"))
    }
}

write_projects_metadata(conn, fields, metadata, projects=NA,
                        project_ids=project_ids, sprint_ids=sprint_ids,
                        output_directory=organization_path,
                        patterns=patterns, join_cols=join_cols)
write_feature_metadata(unique(projects), specifications, organization_path,
                       features=unique(c(feature_names, tag_names, labels,
                                         feature_attributes)),
                       categories=c(), metadata=c('values', 'measurement'))

write(toJSON(results$configuration, auto_unbox=T),
      file=paste(organization_path, "configuration.json", sep="/"))

if (length(results$configuration$organizations) > 1) {
    write(toJSON(results$configuration, auto_unbox=T),
          file=paste(output_directory, "configuration.json", sep="/"))
}

loginfo('Output all project predictions')
