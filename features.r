# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(plyr)
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/metrics.r')
source('include/sources.r')
source('include/tracker.r')
options(warn=1)
conn <- connect()

output_directory <- get_arg('--output', default='output')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
    project_ids <- '1'
}
projects <- strsplit(get_arg('--projects', default=''), ',')[[1]]
features <- get_arg('--features', default=NA)
exclude <- get_arg('--exclude', default='^$')
core <- get_arg('--core', default=F)
recent <- get_arg('--recent', default=F)
story <- get_arg('--story', default=F)
latest_date <- as.POSIXct(get_arg('--latest-date', default=Sys.time()))

config <- get_config()
patterns <- load_definitions('sprint_definitions.yml', config$fields,
                             current_time=latest_date)

project_metadata <- get_arg('--project-metadata', default='recent,core,main')
metadata <- get_meta_keys(project_metadata)
fields <- list('project_id', 'name', 'quality_display_name')

sprint_metadata <- get_arg('--sprint-metadata',
                           default='sprint_name,sprint_num,start_date')
sprint_meta <- strsplit(sprint_metadata, ',')[[1]]

map_details <- function(details, project_ids, component, join_cols) {
    project_col <- join_cols[1]
    sprint_col <- join_cols[2]
    project <- Filter(function(detail) {
                          if (identical(component, F) ||
                              is.null(detail$component)) {
                              cond <- T
                          }
                          else if (is.na(component)) {
                              cond <- !("component" %in% colnames(detail)) |
                                  is.null(detail$component) |
                                  is.na(detail$component)
                          }
                          else {
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
    }
    else {
        names(feature_details) <- Map(function(detail) { detail[[sprint_col]] },
                                      project)
    }
    return(feature_details[!duplicated(names(feature_details))])
}

filter_sprint_details <- function(feature_details, sprint_ids) {
    return(feature_details[names(feature_details) %in% sprint_ids])
}

if (get_arg('--project', default=F)) {
    result <- get_project_features(conn, features, exclude, NULL, core=core,
                                   metadata=metadata, project_fields=fields)
    subprojects <- get_subprojects(conn)
    project_col <- result$join_cols[1]

    df <- result$data[, result$colnames]
    data <- lapply(as.list(split(df, seq(nrow(df)))), unbox)
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
    if (project_ids == '1') {
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
    }
    else {
        links <- list()
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
                            output_directory=output_directory,
                            join_cols=result$join_cols)
} else if (recent) {
    if (isTRUE(recent)) {
        recent <- 5
    }
    split <- get_arg('--split', default=F)
    with_old <- get_arg('--old', default=F)
    futures <- get_arg('--future', default=0)
    closed <- get_arg('--closed', default=F)
    combine <- get_arg('--combine', default='')
    details <- get_arg('--details', default=T)
    scores <- get_arg('--scores', default=F)
    if (combine == '') {
        combine <- F
    }

    if (get_arg('--teams', default=F)) {
        metadata$team <- T
        metadata$component <- T
        teams <- config$teams
    }
    else {
        teams <- list()
    }
    prediction <- get_arg('--prediction', default='')
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
    }
    else {
        features <- expand_feature_names(features, specifications$files,
                                         specifications$categories)
    }

    if (split) {
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
    extra_features <- features[!(features %in% default_features)]
    prediction <- list(data=str_interp(prediction, config$fields),
                       combine=config$fields$prediction_combine,
                       source=config$fields$prediction_url)
    if (prediction$data != '') {
        extra_features <- c(extra_features, 'prediction')
    }
    old_features <- unique(c(sprint_meta, default_features, extra_features))

    core <- get_arg('--core', default=F)
    sprint_days <- get_arg('--days', default=NA)
    sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
    # Latest date condition is handled by recent sprint features itself
    sprint_conditions <- get_sprint_conditions(latest_date='', core=core,
                                               sprint_days=sprint_days,
                                               sprint_patch=sprint_patch,
                                               future=futures > 0)
    variables <- list(project_ids=project_ids)
    result <- get_recent_sprint_features(conn,
                                         unique(c(meta_features, features)),
                                         exclude,
                                         limit=recent,
                                         closed=closed,
                                         sprint_conditions=sprint_conditions,
                                         project_fields=fields,
                                         project_meta=metadata,
                                         old=with_old,
                                         future=futures,
                                         details=split && details,
                                         combine=combine,
                                         teams=teams,
                                         project_names=projects,
                                         components=config$components,
                                         prediction=prediction,
                                         scores=scores,
                                         latest_date=latest_date,
                                         variables=variables)
    default_features <- default_features[default_features %in% result$colnames]
    old_features <- old_features[old_features %in% result$colnames]
    future_features <- future_features[future_features %in% result$colnames]
    extra_features <- extra_features[extra_features %in% result$colnames]
    sprint_meta <- sprint_meta[sprint_meta %in% result$colnames]

    details_features <- list()

    if (!identical(combine, F)) {
        project_column <- "team_id"
    }
    else {
        project_column <- result$join_cols[1]
    }
    sprint_column <- result$join_cols[2]
    if (project_ids != '0') {
        result$data$project_name <- paste("Proj", result$data[, project_column],
                                          sep="")
    }
    sprint_data <- arrange(result$data, result$data$project_name,
                           result$data$start_date)
    if (split) {
        default_features <- c(default_features, result$join_cols[2])
        old_features <- c(old_features, result$join_cols[2])
        sprint_meta <- c(sprint_meta, result$join_cols[2])
        output_dir <- paste(output_directory, 'recent_sprint_features', sep="/")
        if (!dir.exists(output_dir)) {
            dir.create(output_dir)
        }
        if (project_ids != '0') {
            names <- paste('Proj', result$projects$project_id, sep='')
        }
        else {
            names <- result$projects$name
        }
        projects <- levels(factor(names))
        ids <- unique(do.call("c", result$projects$project_ids))

        source_urls <- get_source_urls(conn, ids, multi=T)
        source_ids <- get_source_ids(conn, ids)
        targets <- get_metric_targets(conn, ids, result$items)

        old_sprint_data <- sprint_data[sprint_data$old, ]
        new_sprint_data <- sprint_data[!sprint_data$old & !sprint_data$future, ]
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
            old <- write_data(old_sprint_data, old_features, 'old.json')
            future <- write_data(future_sprint_data, future_features,
                                 'future.json')

            for (feature in extra_features) {
                values <- as.list(new[[feature]])
                names(values) <- NULL
                write(toJSON(values, auto_unbox=T,
                             na="null", null="null"),
                      file=paste(project_dir,
                                   paste(feature, 'json', sep='.'),
                                 sep='/'))
            }

            # There may be multiple original project IDs for team projects.
            if (length(new[[project_column]]) == 0) {
                team_ids <- c()
                quality_name <- NA
                sprint <- list()
                component <- NA
                project_id <- result$projects[names == project, 'project_id']
                team_projects <- c()
                components <- c()
            }
            else {
                team_id <- new[[project_column]][[1]]
                meta <- result$projects[result$projects$project_id == team_id, ]
                result$projects[result$projects$project_id == team_id,
                                'num_sprints'] <- nrow(old) + nrow(new)
                future_dates <- result$errors[[project]]$date
                num_future_sprints <- max(c(nrow(future), length(future_dates)))
                result$projects[result$projects$project_id == team_id,
                                'future_sprints'] <- num_future_sprints
                project_id <- meta$project_ids[[1]]
                team_projects <- meta$project_names[[1]]
                components <- meta$component[[1]]
                if (length(team_projects) == 1 ||
                    (length(components) > 0 && !identical(components, F))) {
                    team_ids <- unlist(c(new$team_id[[1]], project_id))
                }
                else {
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
                }
                else if (length(team_projects) > 1) {
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

                write(toJSON(new_details),
                      file=paste(project_dir, "details.json", sep="/"))
                write(toJSON(old_details),
                      file=paste(project_dir, "details.old.json", sep="/"))
                for (detail in names(all_details)) {
                    if (!(detail %in% default_details)) {
                        write(toJSON(all_details[[detail]]),
                              file=paste(project_dir, paste("details", detail,
                                                            "json", sep="."),
                                         sep="/"))
                    }
                }
            }

            write(toJSON(result$errors[[project]]),
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
            }
            else {
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
        write(toJSON(list(limit=recent, closed=closed,
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
        write(toJSON(list(default=setdiff(default_features, sprint_meta),
                          all=known_features,
                          future=setdiff(future_features, sprint_meta),
                          details=unlist(unique(details_features)),
                          metrics=metrics,
                          meta=sprint_meta)),
              file=paste(output_dir, "features.json", sep="/"))
        write(toJSON(get_expressions_metadata(result$items, sprint_data),
                     auto_unbox=T),
              file=paste(output_dir, "expressions.json", sep="/"))
        default_targets <- get_metric_targets(conn, NA, result$items)
        write_metric_targets(default_targets, output_dir, result$items)
        if (isTRUE(metadata$team) && project_ids != '1') {
            metadata$project_names <- T
        }
        metadata[c('num_sprints', 'future_sprints')] <- T
        write_projects_metadata(conn, result$project_fields, metadata,
                                projects=result$projects,
                                project_ids=project_ids,
                                output_directory=output_dir)
    }
    else {
        columns <- c("project_name", "quality_display_name", sprint_meta)
        write.csv(sprint_data[, columns],
                  file=paste(output_directory, 'recent_sprint_features.csv',
                             sep="/"),
                  row.names=F)
    }
} else if (story) {
    changelog <- !get_arg('--latest', default=F)
    result <- get_story_features(conn, features, exclude, latest_date,
                                 changelog=changelog, project_fields=fields,
                                 project_meta=metadata, project_names=projects)
    story_data <- result$data

    write.arff(story_data,
               file=paste(output_directory, "story_features.arff", sep="/"),
               relation="story_data")
} else {
    days <- get_arg('--days', default=NA)
    patch <- ifelse(get_arg('--patch', default=F), NA, F)
    combine <- get_arg('--combine', default=F)
    details <- get_arg('--details', default=F)
    time <- get_arg('--time', default=F)

    result <- get_sprint_features(conn, features, exclude, NULL, latest_date,
                                  core=core,
                                  sprint_days=days, sprint_patch=patch,
                                  combine=combine, details=details, time=time)
    sprint_data <- result$data

    write.arff(sprint_data[, result$colnames],
               file=paste(output_directory, "sprint_features.arff", sep="/"),
               relation="sprint_data")

    if (details) {
        write(toJSON(result$details),
              file=paste(output_directory, "details.json", sep="/"))
    }
}
