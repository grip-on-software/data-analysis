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

sprint_cache <- vector("list")
get_sprint <- function(project_id, sprint_id, cache=T) {
	if (length(sprint_cache) >= project_id && mode(sprint_cache[[project_id]]) == "list") {
		loginfo('Using cached sprints for project %d', project_id)
		return(sprint_cache[[project_id]][sprint_id,])
	}
	query <- 'SELECT project.name AS project_key, project.quality_name,
				project.quality_display_name, sprint.sprint_id, sprint.name,
				sprint.start_date, ${sprint_close} AS close_date,
				sprint.board_id FROM gros.sprint
				JOIN gros.project ON sprint.project_id = project.project_id
				WHERE sprint.project_id = ${project_id}
				ORDER BY sprint.start_date'
	item <- load_query(list(query=query), c(patterns, project_id=project_id))
	time <- system.time(sprint <- dbGetQuery(conn, item$query))
	loginfo('Obtained sprints of project %d in %f seconds',
			project_id, time['elapsed'])
	if (cache) {
		sprint_cache[[project_id]] <<- sprint
	}
	return(sprint[sprint_id,])
}

input_file <- get_arg('--file', default='sprint_labels.json')
feature_file <- get_arg('--features', default='output/sprint_features.arff')
output_directory <- get_arg('--output', default='output')
config_file <- get_arg('--config', default='config.yml')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids <- '1'
}

results <- read_json(input_file, simplifyVector=T)
features <- read.arff(feature_file)
conn <- connect()

sprint_projects <- get_sprint_projects(conn)
projects <- list()
patterns <- load_definitions('sprint_definitions.yml')
specifications <- yaml.load_file('sprint_features.yml')
config <- yaml.load_file(config_file)
patterns <- load_definitions('sprint_definitions.yml', config$fields)

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%d %H:%M:%S")
}

get_tags <- function(features_row) {
	tags <- list()
	for (file in specifications$files) {
		if (!is.null(file$tags)) {
			tags <- c(tags, file$column[as.logical(features_row[[file$column]])])
		}
	}
	return(tags)
}

for (idx in 1:length(results$projects)) {
	project_id <- results$projects[idx]
	if (project_ids != '1') {
		project_name <- sprint_projects[sprint_projects$project_id == project_id,'name']
	}
	else {
		project_name <- paste("Proj", project_id, sep="")
	}
	projects <- c(projects, project_name)
	sprint_id = results$sprints[idx]
	sprint <- get_sprint(project_id, sprint_id,
						 cache=!is.null(results$analogy_indexes))

	feature_names <- intersect(results$configuration$features, names(features))
	tag_names <- get_tags(setNames(rep(T, length(features)), names(features)))
	feature_excludes <- c("project_id", "sprint_num", tag_names)
	feature_mask <- !(names(features) %in% feature_excludes)
	if (!is.null(results$analogy_indexes)) {
		analogies <- mapply(function(i) {
			analogy <- results$analogy_indexes[idx,i]
			analogy_sprint <- get_sprint(features[analogy,"project_id"],
										 features[analogy,"sprint_num"])
			analogy_values <- as.list(results$analogy_values[idx,i,])
			names(analogy_values) <- as.character(results$configuration$features)
			analogy_value <- modifyList(analogy_values,
										as.list(features[analogy,feature_mask]),
										keep.null=T)
			return(list(project=analogy_sprint$quality_display_name,
						project_id=analogy_sprint$project_key,
						sprint=features[analogy,"sprint_num"],
						id=analogy_sprint$sprint_id,
						name=analogy_sprint$name,
						start_date=as.POSIXct(analogy_sprint$start_date),
						end_date=as.POSIXct(analogy_sprint$close_date),
						label=results$analogy_labels[idx,i],
						features=safe_unbox(analogy_value),
						tags=get_tags(features[analogy,])))
		}, 1:length(results$analogy_indexes[idx,]), SIMPLIFY=F)
	}
	else {
		analogies <- NULL
	}

	sprint_features <- as.list(results$features[idx,])
	names(sprint_features) <- as.character(results$configuration$features)
	features_row <- features[features$project_id==project_id & 
							 features$sprint_num==sprint_id,]
	all_features <- modifyList(sprint_features,
							   as.list(features_row[,feature_mask]),
							   keep.null=T)
	project_data <- list(project=sprint$quality_display_name,
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
						 sources=get_tracker_dates(conn, project_id, aggregate=max))

	path <- paste(output_directory, project_name, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}
	write(toJSON(project_data, auto_unbox=T, na="null", null="null"),
		  file=paste(path, "latest.json", sep="/"))

	source_urls <- get_source_urls(conn, project_id)
	source_items <- list()
	for (item in specifications$files) {
		if (is.list(item$source)) {
			url_names <- paste(names(item$source), 'url', sep='_')
			index = which(url_names %in% names(source_urls))
			if (length(index) > 0) {
				source_items[[item$column]] <- c(item,
												 list(source=item$source[[index[1]]]))
			}
		}
		else if (!is.null(item$source)) {
			source_items[[item$column]] <- item
		}
	}

	sprint_patterns <- list(jira_board_id=sprint$board_id,
							jira_sprint_id=sprint$sprint_id,
							quality_name=ifelse(is.na(sprint$quality_name), '',
												sprint$quality_name),
							sprint_start_date=dateFormat(sprint$start_date),
							sprint_end_date=dateFormat(sprint$close_date))
	links <- build_source_urls(project_id, project_name, items=source_items,
							   patterns=c(patterns, sprint_patterns))
	write(toJSON(links), file=paste(path, "links.json", sep="/"))
}
write(toJSON(get_feature_locales(specifications$files)),
	  file=paste(output_directory, "descriptions.json", sep="/"))
write(toJSON(get_feature_locales(specifications$files, 'units')),
	  file=paste(output_directory, "units.json", sep="/"))
write(toJSON(get_feature_locales(specifications$files, 'tags')),
	  file=paste(output_directory, "tags.json", sep="/"))
write(toJSON(projects, auto_unbox=T),
	  file=paste(output_directory, "projects.json", sep="/"))
write(toJSON(results$configuration, auto_unbox=T),
	  file=paste(output_directory, "configuration.json", sep="/"))

loginfo('Output all project predictions')
