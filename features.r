# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(plyr)
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/sources.r')
source('include/tracker.r')
options(warn=1)
conn <- connect()

output_directory <- get_arg('--output', default='output')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
    project_ids <- '1'
}
features <- get_arg('--features', default=NA)
exclude <- get_arg('--exclude', default='^$')
core <- get_arg('--core', default=F)
recent <- get_arg('--recent', default=F)

config <- get_config()
patterns <- load_definitions('sprint_definitions.yml', config$fields)

project_metadata <- get_arg('--project-metadata', default='recent,core,main')
metadata <- get_meta_keys(project_metadata)
fields <- c('project_id', 'name', 'quality_display_name')

map_details <- function(details, project_ids, sprint_ids) {
    project <- Filter(function(detail) {
                          return(detail$project_id %in% project_ids &&
                                 detail$sprint_id %in% sprint_ids)
                      },
                      details)
    feature_details <- Map(function(detail) {
                               detail$project_id <- NULL
                               detail$sprint_id <- NULL
                               return(unbox(detail))
                           },
                           project)
    names(feature_details) <- Map(function(detail) { detail$sprint_id },
                                  project)
    return(feature_details)
}

if (get_arg('--project', default=F)) {
    result <- get_project_features(conn, features, exclude, NULL, core=core)
    subprojects <- get_subprojects(conn)

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
        num <- result$data$project_id[result$data$name %in% names(data)]
        names(data) <- paste("Proj", sep="")
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
                        result$data[['project_id']],
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

    write(toJSON(get_feature_locales(result$items)),
          file=paste(output_directory, "project_features_locales.json",
                       sep="/"))
    loginfo("Wrote project_features_locales.json")

    write(toJSON(get_locales(yaml.load_file("source_types.yml"))),
          file=paste(output_directory, "project_features_sources.json",
                       sep="/"))
    loginfo("Wrote project_features_sources.json")

    groups <- list()
    for (item in result$items) {
        groups[[item$column]] <- item$groups
    }
    write(toJSON(groups),
          file=paste(output_directory, "project_features_groups.json", sep="/"))
    loginfo("Wrote project_features_groups.json")

    write_projects_metadata(conn, fields, metadata, projects=NA,
                            project_ids=project_ids,
                            output_directory=output_directory)
} else if (recent) {
    if (isTRUE(recent)) {
        recent <- 5
    }
    split <- get_arg('--split', default=F)
    with_old <- get_arg('--old', default=F)
    closed <- get_arg('--closed', default=F)
    combine <- get_arg('--combine', default='')
    if (combine == '') {
        combine <- F
    }

    if (get_arg('--teams', default=F)) {
        metadata$team <- T
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
    if (is.na(features)) {
        features <- all_features
    }
    else {
        features <- strsplit(features, ",")[[1]]
    }

    if (split) {
        sprint_meta <- c('sprint_name', 'sprint_num', 'sprint_id', 'board_id',
                         'start_date', 'close_date')
    }
    else {
        sprint_meta <- c('sprint_name', 'sprint_num', 'start_date', '')
    }

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
    cat_features <- list()

    core <- get_arg('--core', default=F)
    sprint_days <- get_arg('--days', default=NA)
    sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
    conditions <- get_sprint_conditions(latest_date='', core=core,
                                        sprint_days=sprint_days,
                                        sprint_patch=sprint_patch)
    if (length(conditions) != 0) {
        sprint_conditions <- paste('AND', paste(conditions, collapse=' AND '))
    }
    else {
        sprint_conditions <- ''
    }
    result <- get_recent_sprint_features(conn,
                                         unique(c(meta_features, features)),
                                         limit=recent,
                                         closed=closed,
                                         sprint_meta=sprint_meta,
                                         sprint_conditions=sprint_conditions,
                                         project_fields=fields,
                                         project_meta=metadata,
                                         old=with_old,
                                         details=split,
                                         combine=combine,
                                         teams=teams,
                                         prediction=prediction)

    for (cat in names(specifications$categories)) {
        cat_mask <- sapply(result$items, function(item) {
            return("category" %in% names(item) && item$category == cat)
        })
        if (length(cat_mask) > 0) {
            for (item in result$items[cat_mask]) {
                for (column in item$column) {
                    if (!(column %in% old_features)) {
                        cat_features[[cat]] <- c(cat_features[[cat]], column)
                    }
                }
            }
        }
    }
    old_features <- c(old_features, unlist(cat_features))
    details_features <- c()

    sprint_data <- arrange(result$data, result$data$project_name,
                           result$data$start_date)
    if (project_ids != '0') {
        sprint_data$project_name <- paste("Proj", sprint_data$project_id,
                                          sep="")
    }
    if (split) {
        output_dir <- paste(output_directory, 'recent_sprint_features', sep="/")
        if (!dir.exists(output_dir)) {
            dir.create(output_dir)
        }
        projects <- levels(factor(sprint_data$project_name))
        ids <- unique(do.call("c", result$projects$project_ids))

        source_urls <- get_source_urls(conn, ids, multi=T)
        source_ids <- get_source_ids(conn, ids)

        old_sprint_data <- sprint_data[sprint_data$old, ]
        new_sprint_data <- sprint_data[!sprint_data$old, ]

        for (project in projects) {
            project_dir <- paste(output_dir, project, sep="/")
            if (!dir.exists(project_dir)) {
                dir.create(project_dir)
            }
            new <- new_sprint_data[new_sprint_data$project_name == project, ]
            old <- old_sprint_data[old_sprint_data$project_name == project, ]

            loginfo("Writing data for project %s", project)

            write(toJSON(new[, default_features], auto_unbox=T),
                  file=paste(project_dir, 'default.json', sep='/'))
            write(toJSON(old[, old_features], auto_unbox=T),
                  file=paste(project_dir, 'old.json', sep='/'))

            for (feature in extra_features) {
                write(toJSON(new[[feature]], auto_unbox=T,
                             na="null", null="null"),
                      file=paste(project_dir,
                                   paste(feature, 'json', sep='.'),
                                 sep='/'))
            }
            for (cat in names(specifications$categories)) {
                for (column in cat_features[[cat]]) {
                    write(toJSON(new[[column]], auto_unbox=T,
                                 na="null", null="null"),
                          file=paste(project_dir,
                                       paste(column, 'json', sep='.'),
                                     sep='/'))
                }
            }

            # There may be multiple original project IDs for team projects.
            project_id <- result$projects[result$projects$name == project,
                                          'project_ids'][[1]]
            team_projects <- result$team_projects[[project]]
            if (length(team_projects) == 1) {
                team_ids <- c(new$team_id[[1]], project_id[[1]])
            }
            else {
                team_ids <- new$team_id[[1]]
            }
            # Get latest sprint properties
            sprint <- c(new[nrow(new), sprint_meta],
                        list(quality_name=new$quality_name[[1]]))

            new_details <- lapply(result$details, map_details,
                                  team_ids, unlist(new$sprint_id))
            old_details <- lapply(result$details, map_details,
                                  team_ids, unlist(old$sprint_id))
            details_features <- c(details_features, names(new_details))
            default_details <- default[default %in% names(new_details)]
            write(toJSON(new_details[default_details]),
                  file=paste(project_dir, "details.json", sep="/"))
            write(toJSON(old_details[default_details]),
                  file=paste(project_dir, "details.old.json", sep="/"))
            for (detail in names(new_details)) {
                if (!(detail %in% default_details)) {
                    write(toJSON(c(old_details[[detail]],
                                   new_details[[detail]])),
                          file=paste(project_dir,
                                     paste("details", detail, "json", sep="."),
                                     sep="/"))
                }
            }

            project_urls <- source_urls[as.character(project_id)]
            names(project_urls) <- NULL
            project_urls <- do.call("c", project_urls)
            write(toJSON(build_sprint_source_urls(project_urls, project_id,
                                                  project, sprint$quality_name,
                                                  NULL, result$items,
                                                  patterns, team_projects)),
                  file=paste(project_dir, "links.json", sep="/"))

            project_source_ids <- source_ids[as.character(project_id)]
            names(project_source_ids) <- NULL
            write(toJSON(do.call("c", project_source_ids)),
                  file=paste(project_dir, "source_ids.json", sep="/"))

            dates <- get_tracker_dates(conn, project_id, aggregate=max)
            urls <- build_project_source_urls(project_urls, project_id, project,
                                              sprint,
                                              team_projects=team_projects)
            write(toJSON(mapply(function(date, url) {
                                    list(date=unbox(date), url=unbox(url))
                                },
                                dates, urls[names(dates)],
                                USE.NAMES=T, SIMPLIFY=F)),
                  file=paste(project_dir, "sources.json", sep="/"))
        }

        known_features <- c(default_features, extra_features,
                            unlist(cat_features))
        shown_features <- known_features[!(known_features %in% sprint_meta)]
        write_feature_metadata(projects, specifications, output_dir,
                               features=shown_features, items=result$items)
        write(toJSON(list(limit=recent, closed=closed, old=with_old),
                     auto_unbox=T),
              file=paste(output_dir, "sprints.json", sep="/"))
        write(toJSON(list(default=default_features,
                          all=known_features,
                          details=unique(details_features),
                          meta=sprint_meta)),
              file=paste(output_dir, "features.json", sep="/"))
        write(toJSON(get_expressions_metadata(result$items, sprint_data),
                     auto_unbox=T),
              file=paste(output_dir, "expressions.json", sep="/"))
        write_projects_metadata(conn, fields, metadata,
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
} else {
    latest_date <- get_arg('--latest-date', default='')
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
