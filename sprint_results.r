# R script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.

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

latest_date <- as.POSIXct(get_arg('--latest-date', default=Sys.time()))
sprint_days <- get_arg('--days', default=NA)
sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
core <- get_arg('--core', default=F)

projects <- list()
specifications <- yaml.load_file('sprint_features.yml')
config <- get_config()

join_cols <- c('project_id')
if (config$db$primary_source == "tfs") {
    join_cols <- c('team_id')
}

patterns <- load_definitions('sprint_definitions.yml',
                             c(config$fields, list(join_cols=join_cols)),
                             current_time=latest_date)

get_sprints <- function(conn) {
    conditions <- get_sprint_conditions(latest_date=latest_date, core=core,
                                        sprint_days=sprint_days,
                                        sprint_patch=sprint_patch)
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
    }
    else if (config$db$primary_source == "jira_version") {
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

input_file <- get_arg('--file', default='sprint_labels.json')
feature_file <- get_arg('--features', default='output/sprint_features.arff')
output_directory <- get_arg('--output', default='output')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
    project_ids <- '1'
}

project_metadata <- get_arg('--project-metadata', default='recent,core,main')
metadata <- get_meta_keys(project_metadata)
fields <- list('project_id', 'name')
if (config$db$primary_source != "tfs") {
    fields <- c(fields, 'quality_display_name')
}

results <- read_json(input_file, simplifyVector=T)
if (is.list(results$projects)) {
    logwarn("Results is a multiple-run output which we cannot parse.")
    quit("no", status=0, runLast=F)
}
features <- read.arff(feature_file)

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
    return(list(project=analogy_sprint$quality_display_name,
                project_id=analogy_sprint$project_key,
                sprint=features[analogy, "sprint_num"],
                board_id=analogy_sprint$board_id,
                id=analogy_sprint$sprint_id,
                name=analogy_sprint$name,
                start_date=as.POSIXct(analogy_sprint$start_date),
                end_date=as.POSIXct(analogy_sprint$close_date),
                label=results$analogy_labels[idx, i],
                features=safe_unbox(analogy_value),
                tags=get_tags(features[analogy, ])))
}

if (!is.null(results$configuration$assignments)) {
    assignments <- modifyList(results$configuration$assignments,
                              get_expressions_metadata(specifications$files,
                                                       features))
    results$configuration$assignments <- assignments
}

sprint_data <- get_sprint_projects(conn, patterns=patterns,
                                   join_cols=join_cols)
project_col <- join_cols[1]
for (idx in 1:length(results$projects)) {
    project_id <- results$projects[idx]
    sprint_ids <- sort(results$sprints[results$projects == project_id])
    if (project_ids != '1') {
        project_name <- sprint_data[sprint_data[[project_col]] == project_id,
                                    'name']
    }
    else {
        project_name <- paste("Proj", project_id, sep="")
    }
    projects <- c(projects, project_name)
    sprint_id <- results$sprints[idx]
    sprint <- get_sprint_by_id(project_id, sprint_id)

    feature_sets <- intersect(results$configuration$features, names(features))
    tag_names <- get_tags(setNames(rep(T, length(features)), names(features)))
    feature_excludes <- c("project_id", "sprint_num", tag_names)
    feature_mask <- !(names(features) %in% feature_excludes)
    feature_names <- as.character(results$configuration$features)
    if (!is.null(results$analogy_indexes) &&
        nrow(results$analogy_indexes) >= idx) {
        analogies <- mapply(get_analogy_results,
                            1:length(results$analogy_indexes[idx, ]),
                            MoreArgs=list(idx=idx), SIMPLIFY=F)
    }
    else {
        analogies <- NULL
    }

    sprint_features <- as.list(results$features[idx, ])
    names(sprint_features) <- feature_names
    features_row <- features[features[[project_col]] == project_id &
                             features$sprint_id == sprint_id, ]
    all_features <- modifyList(sprint_features,
                               as.list(features_row[, feature_mask]),
                               keep.null=T)
    project_data <- list(project=sprint$quality_display_name,
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
                         sources=get_tracker_dates(conn, project_id,
                                                   aggregate=max))

    path <- paste(output_directory, project_name, sep="/")
    if (!dir.exists(path)) {
        dir.create(path)
    }
    data <- toJSON(project_data, auto_unbox=T, na="null", null="null")
    if (all(sprint_id <= sprint_ids)) {
        write(data, file=paste(path, "latest.json", sep="/"))
        project_sprints <- get_project(project_id)
        project_sprints$sprint_num <- row(project_sprints)[, 1]
        write(toJSON(project_sprints[project_sprints$sprint_id %in% sprint_ids,
                                     c('name', 'sprint_num', 'sprint_id')]),
              file=paste(path, "sprints.json", sep="/"))
    }
    write(data, file=paste(path, paste(sprint_id, "json", sep="."), sep="/"))

    source_urls <- get_source_urls(conn, project_id)
    source_patterns <- list(quality_name=sprint$quality_name)
    write(toJSON(build_project_source_urls(source_urls, project_id,
                                           project_name, source_patterns),
                 auto_unbox=T), file=paste(path, "sources.json", sep="/"))

    source_data <- toJSON(build_sprint_source_urls(source_urls, project_id,
                                                   project_name,
                                                   sprint$quality_name, sprint,
                                                   specifications$files,
                                                   patterns))
    write(source_data, file=paste(path,
                                  paste("links", sprint_id, "json", sep="."),
                                  sep="/"))
    if (all(sprint_id <= sprint_ids)) {
        write(source_data, file=paste(path, "links.json", sep="/"))
    }
}

write_projects_metadata(conn, fields, metadata, projects=NA,
                        project_ids=project_ids,
                        output_directory=output_directory,
                        patterns=patterns, join_cols=join_cols)
write_feature_metadata(unique(projects), specifications, output_directory)
write(toJSON(results$configuration, auto_unbox=T),
      file=paste(output_directory, "configuration.json", sep="/"))

loginfo('Output all project predictions')
