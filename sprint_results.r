# R script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/project.r')

input_file <- get_arg('--file', default='sprint_labels.json')
output_directory <- get_arg('--output', default='output')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids <- '1'
}

results <- read_json(input_file, simplifyVector=T)

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
	query <- paste('SELECT sprint.start_date, ${sprint_close} AS close_date FROM gros.sprint WHERE sprint.project_id = ', project_id, ' ORDER BY sprint.start_date', sep='')
	item <- load_query(list(query=query), patterns)
	sprint <- dbGetQuery(conn, item$query)
	path <- paste(output_directory, project_name, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}
	sprint_id = results$sprints[idx]
	project_data <- list(project=project_name, sprint=sprint_id,
						 start_date=as.POSIXct(sprint$start_date[sprint_id]),
						 end_date=as.POSIXct(sprint$close_date[sprint_id]),
						 prediction=results$labels[idx],
						 probability=results$probabilities[idx],
						 risk=results$risks[idx],
						 metrics=results$metrics,
						 configuration=results$configuration)
	write(toJSON(project_data, auto_unbox=T),
		  file=paste(path, "latest.json", sep="/"))
}
write(toJSON(projects, auto_unbox=T),
	  file=paste(output_directory, "projects.json", sep="/"))

loginfo('Output all project predictions')
