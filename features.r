# Script that extracts features regarding sprints or stories from the database
# and exports them to an ARFF file readable by Weka and other data mining tools,
# or recent/split data to JSON or CSV files, or features regarding projects
# to JSON files.
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

library(foreign) # For write.arff
library(plyr)
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/log.r')
source('include/metrics.r')
source('include/sources.r')
source('include/tracker.r')
options(warn=1)

make_opt_parser(desc="Extract features and export them into ARFF, JSON or CSV",
                options=list(make_option('--output', default='output',
                                         help='Output directory'),
                             make_option('--project-ids', default='0',
                                         help='Anonymize projects (0 or 1)'),
                             make_option('--sprint-ids', default='0',
                                         help='Anonymize sprints (0 or 1)'),
                             make_option('--projects', default='',
                                         help='List of projects to collect'),
                             make_option('--features', default=NA_character_,
                                         help='List of features to collect'),
                             make_option('--exclude', default='^$',
                                         help=paste('Regular expression of',
                                                    'features to filter')),
                             make_option('--core', action='store_true',
                                         default=FALSE,
                                         help=paste('Only consider non-support',
                                                    'team, main projects')),
                             make_option('--recent', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect recent sprints',
                                                    '(exported as JSON/CSV)')),
                             make_option('--limit', default=5,
                                         help='Number of recent sprints'),
                             make_option('--split', action='store_true',
                                         default=FALSE,
                                         help=paste('Export older sprints',
                                                    'separate from recent',
                                                    '(exported as JSON)')),
                             make_option('--old', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect older sprints',
                                                    'next to recent sprints',
                                                    '(exported as JSON)')),
                             make_option('--closed', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect recent sprints',
                                                    'only if they are closed')),
                             make_option('--combine', default='',
                                         help=paste('Combine sprints based on',
                                                    'metadata/feature field',
                                                    'or number of intervals',
                                                    '(recent/sprint only)')),
                             make_option('--details', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect feature details',
                                                    '(recent/sprint only)')),
                             make_option('--extra', action='store_true',
                                         default=TRUE,
                                         help=paste('Collect non-default',
                                                    'recent sprint features')),
                             make_option('--default', default=NA_character_,
                                         help=paste('List of default features',
                                                    'next to the standard ones',
                                                    'for recent sprints')),
                             make_option('--teams', action='store_true',
                                         default=FALSE,
                                         help=paste('Combine projects into',
                                                    'teams (recent only)')),
                             make_option('--prediction', default='',
                                         help=paste('Format for prediction URL',
                                                    '(recent only)')),
                             make_option('--fixversions', default='',
                                         help=paste('List of fix version IDs',
                                                    'to filter issues on',
                                                    '(recent only)')),
                             make_option('--project', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect project features',
                                                    '(exported as JSON)')),
                             make_option('--story', action='store_true',
                                         default=FALSE,
                                         help=paste('Collect story features',
                                                    '(exported as ARFF)')),
                             make_option('--latest', action='store_true',
                                         default=FALSE,
                                         help=paste('Only collect features for',
                                                    'latest changelog version',
                                                    '(story only)')),
                             make_option('--filename', default=NA_character_,
                                         help=paste('File name to export ARFF',
                                                    'to (story/sprint only)')),
                             make_option('--scores', action='store_true',
                                         default=FALSE,
                                         help='Collect feature scores'),
                             make_option('--future', default=0,
                                         help=paste('Generate future sprints',
                                                    'and perform predictions',
                                                    '(recent/story only)')),
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
                             make_option('--days', default=NA_integer_,
                                         help=paste('Number of days before a',
                                                    'sprint is left out',
                                                    '(recent/sprint only)')),
                             make_option('--patch', action='store_true',
                                         default=FALSE,
                                         help=paste('Exclude patch sprints',
                                                    '(recent/sprint only)')),
                             make_option('--time', action='store_true',
                                         default=FALSE,
                                         help=paste('Export time feature',
                                                    '(day that sprint started)',
                                                    'for temporal data set',
                                                    '(sprint only)')),
                             make_option('--append', action='store_true',
                                         default=FALSE,
                                         help=paste('Add data set to end of',
                                                    'ARFF instead of overwrite',
                                                    '(sprint only)')),
                             make_option('--points', action='store_true',
                                         default=TRUE,
                                         help=paste('Filter on sprints with',
                                                    'planned story points',
                                                    '(sprint only)')),
                             make_option('--cache-update', action='store_true',
                                         default=TRUE,
                                         help='Update feature cache tables'),
                             make_option('--project-metadata',
                                         default='recent,core,main',
                                         help=paste('List of project metadata',
                                                    'fields to output')),
                             make_option('--story-metadata',
                                         default=paste(c('project_name',
                                                         'sprint_name',
                                                         'story_name',
                                                         'latest'),
                                                       collapse=','),
                                         help=paste('List of story metadata',
                                                    'fields to output')),
                             make_option('--sprint-metadata',
                                         default=paste(c('sprint_name',
                                                         'sprint_num',
                                                         'start_date'),
                                                       collapse=','),
                                         help=paste('List of sprint metadata',
                                                    'fields to output'))),
                variables=get_config_fields())
config <- get_config()
arguments <- config$args
log_setup(arguments)

conn <- connect()

output_directory <- arguments$output
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
projects <- strsplit(arguments$projects, ',')[[1]]
features <- arguments$features
exclude <- arguments$exclude
core <- arguments$core
story <- arguments$story
scores <- arguments$score
futures <- arguments$future
latest_date <- as.POSIXct(arguments$latest_date)
cache_update <- arguments$cache_update
combine <- arguments$combine
if (combine == '') {
    combine <- F
}
details <- arguments$details

patterns <- load_definitions('sprint_definitions.yml', config$fields,
                             current_time=latest_date)

metadata <- get_meta_keys(arguments$project_metadata, arguments$recent_date)

story_metadata <- strsplit(arguments$story_metadata, ',')[[1]]
fields <- list('project_id', 'name', 'quality_display_name')

sprint_meta <- strsplit(arguments$sprint_metadata, ',')[[1]]

map_details <- function(details, project_ids, component, join_cols) {
    if (identical(details, F)) {
        return(NULL)
    }
    project_col <- join_cols[1]
    sprint_col <- join_cols[2]
    project <- Filter(function(detail) {
                          if (identical(component, F) ||
                              is.null(detail$component)) {
                              cond <- T
                          } else if (is.na(component)) {
                              cond <- !("component" %in% colnames(detail)) |
                                  is.null(detail$component) |
                                  is.na(detail$component)
                          } else {
                              cond <- "component" %in% colnames(detail) &
                                  detail$component == component
                          }
                          return(detail[[project_col]] %in% project_ids & cond)
                      },
                      details)
    feature_details <- Map(function(detail) {
                               detail[[project_col]] <- NULL
                               detail[[sprint_col]] <- NULL
                               detail$component <- NULL
                               detail$original_component <- NULL
                               return(unbox(detail))
                           },
                           project)
    if (length(project) == 0) {
        names(feature_details) <- character(0)
    } else {
        names(feature_details) <- Map(function(detail) { detail[[sprint_col]] },
                                      project)
    }
    return(feature_details[!duplicated(names(feature_details))])
}

filter_sprint_details <- function(feature_details, sprint_ids) {
    return(feature_details[names(feature_details) %in% sprint_ids])
}

if (arguments$project) {
    result <- get_project_features(conn, features, exclude, NULL, core=core,
                                   metadata=metadata, project_fields=fields)
    subprojects <- get_subprojects(conn)
    project_col <- result$join_cols[1]

    df <- result$data[, result$colnames]
    data <- lapply(as.list(split(df, seq_len(nrow(df)))), unbox)
    names(data) <- result$data$name
    for (subproject in subprojects$name) {
        main_project <- subprojects[subprojects$name == subproject,
                                    'main_project']
        if (main_project %in% data) {
            data[[main_project]] <- unbox(data[[main_project]] +
                                          data[[subproject]])
        }
        data[subproject] <- NULL
    }
    if (project_ids != '0') {
        num <- result$data[[project_col]][result$data$name %in% names(data)]
        names(data) <- paste("Proj", num, sep="")
    }
    write(toJSON(data),
          file=paste(output_directory, "project_features.json", sep="/"))
    loginfo("Wrote project_features.json")

    normalize <- lapply(result$items, function(item) { unbox(item$normalize) })
    names(normalize) <- result$colnames
    write(toJSON(normalize, null="null"),
          file=paste(output_directory, "project_features_normalize.json",
                       sep="/"))
    loginfo("Wrote project_features_normalize.json")

    if (project_ids != '1') {
        links <- mapply(build_source_urls,
                        result$data[[project_col]],
                        result$data[['name']],
                        MoreArgs=list(patterns=patterns, items=result$items,
                                      conn=conn),
                        SIMPLIFY=F)
        names(links) <- result$data[['name']]
    } else {
        links <- list()
        names(links) <- list()
    }
    write(toJSON(links),
          file=paste(output_directory, "project_features_links.json", sep="/"))
    loginfo("Wrote project_features_links.json")

    write(toJSON(list(descriptions=get_feature_locales(result$items),
                      sources=get_locales(yaml.load_file("source_types.yml")))),
          file=paste(output_directory, "project_features_localization.json",
                       sep="/"))
    loginfo("Wrote project_features_localization.json")

    groups <- list()
    for (item in result$items) {
        groups[[item$column]] <- item$groups
    }
    write(toJSON(groups),
          file=paste(output_directory, "project_features_groups.json", sep="/"))
    loginfo("Wrote project_features_groups.json")

    write_projects_metadata(conn, result$project_fields, metadata,
                            projects=result$projects,
                            project_ids=project_ids,
                            sprint_ids=sprint_ids,
                            output_directory=output_directory,
                            join_cols=result$join_cols)
} else if (arguments$recent) {
    split <- arguments$split
    with_old <- arguments$old
    closed <- arguments$closed
    extra <- arguments$extra
    more_default <- arguments$default

    if (arguments$teams) {
        metadata$team <- T
        metadata$component <- T
        teams <- config$teams
    } else {
        teams <- list()
    }
    specifications <- yaml.load_file('sprint_features.yml')
    all_features <- unlist(sapply(specifications$files, function(item) {
        if (is.null(item$tag) || is.null(item$expression)) {
            # Filter expressions that are used only as tags
            return(item$column)
        }
    }))
    prediction_features <- unlist(sapply(specifications$files, function(item) {
        if (!is.null(item$prediction)) {
            return(item$column)
        }
    }))
    if (is.na(features)) {
        features <- all_features
    } else {
        features <- expand_feature_names(features, specifications$files,
                                         specifications$categories)
    }

    if (split || with_old) {
        sprint_meta <- c(sprint_meta, 'board_id', 'close_date')
    }

    future_features <- unique(c(sprint_meta, prediction_features, 'future'))
    if (!closed) {
        sprint_meta <- c(sprint_meta, 'sprint_is_closed', 'sprint_is_complete')
    }
    meta_features <- sprint_meta[sprint_meta %in% all_features]
    default <- c(sprint_meta, 'num_story_points', 'done_story_points',
                 'velocity_three', 'lines_of_code', 'unittest_line_coverage')
    default_features <- default[default %in% c(sprint_meta, features)]
    if (!is.na(more_default)) {
        more_default_features <- expand_feature_names(more_default,
                                                      specifications$files,
                                                      specifications$categories)
        default_features <- unique(c(default_features, more_default_features))
        default <- unique(c(default, more_default_features))
    }
    extra_features <- features[!(features %in% default_features)]
    prediction <- list(data=str_interp(arguments$prediction, config$fields),
                       combine=config$fields$prediction_combine,
                       source=config$fields$prediction_url)
    if (prediction$data != '') {
        extra_features <- c(extra_features, 'prediction')
    }
    old_features <- unique(c(sprint_meta, default_features, extra_features))

    sprint_patch <- ifelse(arguments$patch, NA, F)
    fixversions <- strsplit(arguments$fixversions, ',')[[1]]
    # Latest date condition is handled by recent sprint features itself
    sprint_conditions <- get_sprint_conditions(latest_date='', core=core,
                                               sprint_days=arguments$days,
                                               sprint_patch=sprint_patch,
                                               future=futures > 0)
    result <- get_recent_sprint_features(conn,
                                         unique(c(meta_features, features)),
                                         exclude,
                                         date=arguments$recent_date,
                                         limit=arguments$limit,
                                         closed=closed,
                                         sprint_conditions=sprint_conditions,
                                         project_fields=fields,
                                         project_meta=metadata,
                                         old=with_old,
                                         future=futures,
                                         details=details && (split || with_old),
                                         combine=combine,
                                         teams=teams,
                                         project_names=projects,
                                         components=config$components,
                                         prediction=prediction,
                                         scores=scores,
                                         latest_date=latest_date,
                                         variables=variables,
                                         filters=list(fixversion=fixversions),
                                         cache_update=cache_update)
    default_features <- default_features[default_features %in% result$colnames]
    old_features <- old_features[old_features %in% result$colnames]
    future_features <- future_features[future_features %in% result$colnames]
    extra_features <- extra_features[extra_features %in% result$colnames]
    sprint_meta <- sprint_meta[sprint_meta %in% result$colnames]

    details_features <- list()

    if (!identical(combine, F)) {
        project_column <- "team_id"
    } else {
        project_column <- result$join_cols[1]
    }
    sprint_column <- result$join_cols[2]
    if (project_ids != '0') {
        result$data$project_name <- paste("Proj", result$data[, project_column],
                                          sep="")
    }
    sprint_data <- arrange(result$data, result$data$project_name,
                           result$data$start_date)
    if (split || with_old) {
        default_features <- c(default_features, result$join_cols[2])
        old_features <- c(old_features, result$join_cols[2])
        sprint_meta <- c(sprint_meta, result$join_cols[2])
        output_dir <- paste(output_directory, 'recent_sprint_features', sep="/")
        if (!dir.exists(output_dir)) {
            dir.create(output_dir)
        }
        if (project_ids != '0') {
            names <- paste('Proj', result$projects$project_id, sep='')
        } else {
            names <- result$projects$name
        }
        projects <- levels(factor(names))
        ids <- unique(do.call("c", result$projects$project_ids))

        source_urls <- get_source_urls(conn, ids, multi=T)
        source_ids <- get_source_ids(conn, ids)
        targets <- get_metric_targets(conn, ids, result$items)

        if (!split) {
            old_sprint_data <- data.frame()
            new_sprint_data <- sprint_data[!sprint_data$future, ]
        } else {
            old_sprint_data <- sprint_data[sprint_data$old, ]
            new_sprint_data <- sprint_data[!sprint_data$old &
                                           !sprint_data$future, ]
        }
        future_sprint_data <- sprint_data[sprint_data$future, ]

        result$projects[, c('num_sprints', 'future_sprints')] <- 0
        for (project in projects) {
            project_dir <- paste(output_dir, project, sep="/")
            if (!dir.exists(project_dir)) {
                dir.create(project_dir)
            }

            write_data <- function(data, features, name) {
                project_data <- data[data$project_name == project, ]
                write(toJSON(project_data[, features], auto_unbox=T),
                      file=paste(project_dir, name, sep='/'))
                return(project_data)
            }

            loginfo("Writing data for project %s", project)
            new <- write_data(new_sprint_data, default_features, 'default.json')
            if (split) {
                old <- write_data(old_sprint_data, old_features, 'old.json')
            } else {
                old <- data.frame()
            }
            future <- write_data(future_sprint_data, future_features,
                                 'future.json')

            if (extra) {
                for (feature in extra_features) {
                    values <- as.list(new[[feature]])
                    names(values) <- NULL
                    write(toJSON(values, auto_unbox=T,
                                 na="null", null="null"),
                          file=paste(project_dir,
                                       paste(feature, 'json', sep='.'),
                                     sep='/'))
                }
            }

            # There may be multiple original project IDs for team projects.
            if (length(new[[project_column]]) == 0) {
                team_ids <- c()
                quality_name <- NA
                sprint <- list()
                component <- NA
                project_id <- result$projects[names == project, 'project_id']
                team_id <- project_id
                team_projects <- c()
                components <- c()
            } else {
                team_id <- new[[project_column]][[1]]
                meta <- result$projects[result$projects$project_id == team_id, ]
                result$projects[result$projects$project_id == team_id,
                                'num_sprints'] <- nrow(old) + nrow(new)
                project_id <- meta$project_ids[[1]]
                team_projects <- meta$project_names[[1]]
                components <- meta$component[[1]]

                future_dates <- result$errors[[as.character(team_id)]]$date
                num_future_sprints <- max(c(nrow(future), length(future_dates)))
                result$projects[result$projects$project_id == team_id,
                                'future_sprints'] <- num_future_sprints

                if (length(team_projects) == 1 ||
                    (length(components) > 0 && !identical(components, F))) {
                    team_ids <- unlist(c(new$team_id[[1]], project_id))
                } else {
                    team_ids <- new$team_id[[1]]
                }

                # Get latest sprint properties
                quality_name <- new$quality_name[[1]]
                if (is.null(quality_name)) {
                    quality_name <- NA
                }
                sprint <- new[nrow(new), sprint_meta]

                component <- new$component[[1]]
                if (is.null(component)) {
                    component <- NA
                } else if (length(team_projects) > 1) {
                    component <- F
                }
            }
            sprint <- c(sprint, list(quality_name=quality_name))

            if (details) {
                all_details <- lapply(result$details, map_details,
                                      team_ids, component, result$join_cols)

                details_features <- c(details_features, names(all_details))
                default_details <- default[default %in% names(all_details)]

                new_sprint_ids <- unlist(new[[sprint_column]])
                old_sprint_ids <- unlist(old[[sprint_column]])
                new_details <- lapply(all_details[default_details],
                                      filter_sprint_details, new_sprint_ids)
                old_details <- lapply(all_details[default_details],
                                      filter_sprint_details, old_sprint_ids)

                write(toJSON(new_details, na="null"),
                      file=paste(project_dir, "details.json", sep="/"))
                write(toJSON(old_details, na="null"),
                      file=paste(project_dir, "details.old.json", sep="/"))
                for (detail in names(all_details)) {
                    if (!(detail %in% default_details)) {
                        write(toJSON(all_details[[detail]], na="null"),
                              file=paste(project_dir, paste("details", detail,
                                                            "json", sep="."),
                                         sep="/"))
                    }
                }
            }

            write(toJSON(result$errors[[as.character(team_id)]]),
                  file=paste(project_dir, "errors.json", sep="/"))

            project_urls <- source_urls[as.character(project_id)]
            names(project_urls) <- NULL
            project_urls <- do.call("c", project_urls)
            if (project_ids != '1') {
                write(toJSON(build_sprint_source_urls(project_urls, project_id,
                                                      project, quality_name,
                                                      NULL, result$items,
                                                      patterns, team_projects,
                                                      config$components,
                                                      component)),
                      file=paste(project_dir, "links.json", sep="/"))

                project_source_ids <- source_ids[as.character(project_id)]
                names(project_source_ids) <- NULL
                write(toJSON(do.call("c", project_source_ids)),
                      file=paste(project_dir, "source_ids.json", sep="/"))
            } else {
                write("{}", file=paste(project_dir, "links.json", sep="/"))
                write("{}", file=paste(project_dir, "source_ids.json", sep="/"))
            }

            dates <- get_tracker_dates(conn, project_id, aggregate=max)
            urls <- build_project_source_urls(project_urls, project_id, project,
                                              sprint,
                                              team_projects=team_projects,
                                              components=components)
            write(toJSON(mapply(function(date, url) {
                                    if (!is.na(date) && !is.null(url)) {
                                        list(date=date,
                                             url=ifelse(project_ids != '1',
                                                        url, NA))
                                    }
                                },
                                dates, urls[names(dates)],
                                USE.NAMES=T, SIMPLIFY=F), auto_unbox=T),
                  file=paste(project_dir, "sources.json", sep="/"))
            metric_targets <- targets[targets$project_id %in% project_id, ]
            write_metric_targets(metric_targets, project_dir, result$items)
        }

        known_features <- c(default_features, extra_features)
        shown_features <- known_features[!(known_features %in% sprint_meta)]
        write_feature_metadata(projects, specifications, output_dir,
                               features=shown_features, items=result$items,
                               locales=c('descriptions', 'long_descriptions',
                                         'units', 'short_units', 'predictor'))
        write(toJSON(list(limit=arguments$limit, closed=closed,
                          old=with_old, future=futures),
                     auto_unbox=T),
              file=paste(output_dir, "sprints.json", sep="/"))
        metrics <- unlist(lapply(result$items,
                                 function(item) {
                                     if (!is.null(item$metric)) {
                                         return(item$column)
                                     }
                                     return(NULL)
                                 }))
        if (is.null(metrics)) {
            metrics <- list()
        }
        details_meta <- unlist(unique(details_features))
        if (is.null(details_meta)) {
            details_meta <- list()
        }
        write(toJSON(list(default=setdiff(default_features, sprint_meta),
                          all=known_features,
                          future=setdiff(future_features, sprint_meta),
                          details=details_meta,
                          metrics=metrics,
                          meta=sprint_meta)),
              file=paste(output_dir, "features.json", sep="/"))
        write(toJSON(get_expressions_metadata(result$items, sprint_data),
                     auto_unbox=T),
              file=paste(output_dir, "expressions.json", sep="/"))
        default_targets <- get_metric_targets(conn, NA, result$items)
        write_metric_targets(default_targets, output_dir, result$items)
        if (isTRUE(metadata$team)) {
            field <- ifelse(project_ids != '1', 'project_names', 'project_ids')
            metadata[[field]] <- T
        }
        metadata[c('num_sprints', 'future_sprints')] <- T
        write_projects_metadata(conn, result$project_fields, metadata,
                                projects=result$projects,
                                project_ids=project_ids,
                                sprint_ids=sprint_ids,
                                output_directory=output_dir,
                                fixversions=config$db$primary_source == "jira")
    } else {
        columns <- c("project_name", "quality_display_name", sprint_meta)
        write.csv(sprint_data[, columns],
                  file=paste(output_directory, 'recent_sprint_features.csv',
                             sep="/"),
                  row.names=F)
    }
} else if (story) {
    changelog <- !arguments$latest
    filename <- ifelse(is.na(arguments$filename), 'story_features.arff',
                       arguments$filename)

    result <- get_story_features(conn, features, exclude, latest_date,
                                 changelog=changelog, project_fields=fields,
                                 project_meta=metadata, project_names=projects,
                                 scores=scores, future=futures,
                                 variables=variables)
    story_data <- result$data
    colnames <- unique(c(story_metadata, result$colnames))

    write.arff(story_data[, colnames],
               file=paste(output_directory, filename, sep="/"),
               relation="story_data")
} else {
    filename <- ifelse(is.na(arguments$filename), 'sprint_features.arff',
                       arguments$filename)

    result <- get_sprint_features(conn, features, exclude, variables,
                                  latest_date,
                                  core=core, sprint_days=arguments$days,
                                  sprint_patch=arguments$patch,
                                  combine=combine,
                                  details=details,
                                  time=arguments$time, scores=scores,
                                  cache_update=cache_update)
    sprint_data <- result$data[, result$colnames]
    if (arguments$points && 'num_story_points' %in% result$colnames) {
        sprint_data <- result$data[result$data$num_story_points > 0, ]
    }
    if ('team_id' %in% result$colnames) {
        colnames(sprint_data)[1] <- "project_id"
    }

    path <- paste(output_directory, filename, sep="/")
    if (arguments$append) {
        if (nrow(sprint_data) == 0) {
            loginfo('No sprints found for organization %s!', organization)
        } else {
            sprint_data$organization <- as.factor(organization)
        }
        if (file.exists(path)) {
            old_data <- read.arff(path)
            sprint_data <- rbind.fill(old_data, sprint_data)
        }
    }
    write.arff(sprint_data, file=path, relation="sprint_data")

    if (details) {
        write(toJSON(result$details),
              file=paste(output_directory, "details.json", sep="/"))
    }
    if (scores) {
        write(toJSON(result$scores, auto_unbox=T),
              file=paste(output_directory, "scores.json", sep="/"))
    }
}
