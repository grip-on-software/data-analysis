# R script that combines output from a prediction model with other sprint data
# such that it can be used by an API producer.

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')

input_file <- get_arg('--file', default='sprint_labels.json')

results <- read_json(input_file, simplifyVector=T)

conn <- connect()
projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

for (idx in 1:length(results$projects)) {
	project <- results$projects[idx]
	project_name <- projects[project,'name'] 
	path <- paste("output", project_name, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}
	project_data <- list(project=project_name, sprint=results$sprints[idx],
						 prediction=results$labels[idx],
						 probability=results$probabilities[idx],
						 risk=results$risks[idx],
						 metrics=results$metrics,
						 configuration=results$configuration)
	write(toJSON(project_data, auto_unbox=T),
		  file=paste(path, "latest.json", sep="/"))
}

loginfo('Output all project predictions')
