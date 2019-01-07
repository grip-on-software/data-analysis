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

latest_date <- get_arg('--latest-date', default='')
sprint_days <- get_arg('--days', default=NA)
sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
core <- get_arg('--core', default=F)

projects <- list()
specifications <- yaml.load_file('sprint_features.yml')
config <- get_config()
patterns <- load_definitions('sprint_definitions.yml', config$fields)

get_sprints <- function(conn) {
    conditions <- get_sprint_conditions(latest_date, core=core,
                                        sprint_days=sprint_days,
                                        sprint_patch=sprint_patch)
    query <- paste('SELECT project.project_id, project.name AS project_key,
                   project.quality_name, project.quality_display_name,
                   sprint.sprint_id, sprint.name,
                   sprint.start_date, ${s(sprint_close)} AS close_date,
                   sprint.board_id FROM gros.sprint
                   JOIN gros.project ON sprint.project_id = project.project_id
                   WHERE', paste(conditions, collapse=' AND '), '
                   ORDER BY project.project_id, ${s(sprint_open)}, sprint.name')
    item <- load_query(list(query=query), c(patterns, sprint_days=sprint_days))
    time <- system.time(sprint <- dbGetQuery(conn, item$query))
    loginfo('Obtained sprints in %f seconds', time['elapsed'])
    return(sprint)
}

conn <- connect()
sprint_cache <- get_sprints(conn)
get_sprint <- function(project_id, sprint_id) {
    return(sprint_cache[sprint_cache$project_id == project_id, ][sprint_id, ])
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
fields <- c('project_id', 'name', 'quality_display_name')

results <- read_json(input_file, simplifyVector=T)
if (is.list(results$projects)) {
    logwarn("Results is a multiple-run output which we cannot parse.")
    quit("no", status=0, runLast=F)
}
features <- read.arff(feature_file)

get_tags <- function(features_row) {
    tags <- list()
    for (file in specifications$files) {
        if (!is.null(file$tags)) {
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

sprint_data <- get_sprint_projects(conn)
for (idx in 1:length(results$projects)) {
    project_id <- results$projects[idx]
    project_sprints <- results$sprints[results$projects == project_id]
    if (project_ids != '1') {
        project_name <- sprint_data[sprint_data$project_id == project_id,
                                    'name']
    }
    else {
        project_name <- paste("Proj", project_id, sep="")
    }
    projects <- c(projects, project_name)
    sprint_id <- results$sprints[idx]
    sprint <- get_sprint(project_id, sprint_id)

    feature_sets <- intersect(results$configuration$features, names(features))
    tag_names <- get_tags(setNames(rep(T, length(features)), names(features)))
    feature_excludes <- c("project_id", "sprint_num", tag_names)
    feature_mask <- !(names(features) %in% feature_excludes)
    feature_names <- as.character(results$configuration$features)
    if (!is.null(results$analogy_indexes)) {
        analogies <- mapply(get_analogy_results,
                            1:length(results$analogy_indexes[idx, ]),
                            MoreArgs=list(idx=idx), SIMPLIFY=F)
    }
    else {
        analogies <- NULL
    }

    sprint_features <- as.list(results$features[idx, ])
    names(sprint_features) <- feature_names
    features_row <- features[features$project_id==project_id &
                             features$sprint_num==sprint_id, ]
    all_features <- modifyList(sprint_features,
                               as.list(features_row[, feature_mask]),
                               keep.null=T)
    project_data <- list(project=sprint$quality_display_name,
                         project_id=sprint$project_key,
                         sprint=sprint_id,
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
    if (all(sprint_id <= project_sprints)) {
        write(data, file=paste(path, "latest.json", sep="/"))
        write(toJSON(sort(project_sprints)),
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
    if (all(sprint_id <= project_sprints)) {
        write(source_data, file=paste(path, "links.json", sep="/"))
    }
}

write_projects_metadata(conn, fields, metadata, projects=NA,
                        project_ids=project_ids,
                        output_directory=output_directory)
write_feature_metadata(unique(projects), specifications, output_directory)
write(toJSON(results$configuration, auto_unbox=T),
      file=paste(output_directory, "configuration.json", sep="/"))

loginfo('Output all project predictions')
