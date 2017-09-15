# R script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/project.r')

input_file <- get_arg('--file', default='sprint_labels.json')

results <- read_json(input_file, simplifyVector=T)

conn <- connect()
projects <- get_sprint_projects(conn)

write(toJSON(projects$name, auto_unbox=T),
	  file=paste("output", "projects.json", sep="/"))
patterns <- load_definitions('sprint_definitions.yml')
for (idx in 1:length(results$projects)) {
	project <- results$projects[idx]
	project_name <- projects[project,'name'] 
	query <- paste('SELECT sprint.start_date, ${sprint_close} AS close_date FROM gros.sprint WHERE sprint.project_id = ', projects[project,'project_id'], ' ORDER BY sprint.start_date', sep='')
	item <- load_query(list(query=query), patterns)
	sprint <- dbGetQuery(conn, item$query)
	path <- paste("output", project_name, sep="/")
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

loginfo('Output all project predictions')
