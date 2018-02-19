# R script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.

library(foreign)
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/project.r')

sprint_cache <- vector("list")
get_sprint <- function(project_id, sprint_id, cache=T) {
	if (length(sprint_cache) >= project_id && mode(sprint_cache[[project_id]]) == "list") {
		loginfo('Using cached sprints for project %d', project_id)
		return(sprint_cache[[project_id]][sprint_id,])
	}
	query <- 'SELECT project.name AS project_key, project.quality_display_name, sprint.sprint_id, sprint.name, sprint.start_date, ${sprint_close} AS close_date FROM gros.sprint JOIN gros.project ON sprint.project_id = project.project_id WHERE sprint.project_id = ${project_id} ORDER BY sprint.start_date'
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
	if (!is.null(results$analogy_indexes)) {
		analogies <- mapply(function(analogy, label) {
			analogy_sprint <- get_sprint(features[analogy,"project_id"],
										 features[analogy,"sprint_num"])
			return(list(project=analogy_sprint$quality_display_name,
						project_id=analogy_sprint$project_key,
						sprint=features[analogy,"sprint_num"],
						id=analogy_sprint$sprint_id,
						name=analogy_sprint$name,
						start_date=as.POSIXct(analogy_sprint$start_date),
						end_date=as.POSIXct(analogy_sprint$close_date),
						label=label,
						features=features[analogy,feature_names]))
		}, results$analogy_indexes[idx,], results$analogy_labels[idx,],
		SIMPLIFY=F)
	}
	else {
		analogies <- NULL
	}

	path <- paste(output_directory, project_name, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}
	project_data <- list(project=sprint$quality_display_name,
						 sprint=sprint_id,
						 id=sprint$sprint_id,
						 name=sprint$name,
						 start_date=as.POSIXct(sprint$start_date),
						 end_date=as.POSIXct(sprint$close_date),
						 prediction=results$labels[idx],
						 probability=results$probabilities[idx],
						 risk=results$risks[idx],
						 metrics=results$metrics,
						 analogies=analogies,
						 features=results$features[idx],
						 configuration=results$configuration)
	write(toJSON(project_data, auto_unbox=T, null="null"),
		  file=paste(path, "latest.json", sep="/"))
}
write(toJSON(projects, auto_unbox=T),
	  file=paste(output_directory, "projects.json", sep="/"))

loginfo('Output all project predictions')
