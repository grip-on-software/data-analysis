# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
conn <- connect()

exclude <- get_arg('--exclude', '^$')
if (get_arg('--project', F)) {
	result <- get_project_features(conn, exclude)
	df <- result$data[,result$colnames]
	data <- lapply(as.list(split(df, seq(nrow(df)))), unbox)
	names(data) <- result$data[['name']]
	write(toJSON(data), file="output/project_features.json")

	config <- yaml.load_file('config.yml')
	patterns <- load_definitions('sprint_definitions.yml', config$fields)
	links <- mapply(function(project_id, project) {
		project_links <- list()
		project_environments <- dbGetQuery(conn, paste('SELECT source_type, url FROM gros.source_environment WHERE project_id = ', project_id, sep=''))
		print(project_environments)
		project_patterns <- c(patterns, list(jira_key=project))

		for (item in result$items) {
			source_url <- str_interp(item$source, project_patterns)
			project_links[[item$column]] <- list(source=unbox(source_url))
		}
		return(project_links)
	}, result$data[['project_id']], result$data[['name']], SIMPLIFY=F)
	names(links) <- result$data[['name']]
	write(toJSON(links), file="output/project_features_links.json")

	locale <- list()
	for (item in result$items) {
		locale[[item$column]] <- unbox(item$name)
	}
	write(toJSON(locale), file="output/project_features_localization.json")
} else {
	result <- get_sprint_features(conn, exclude)
	sprint_data <- result$data

	write.arff(sprint_data[,result$colnames],
			   file='output/sprint_features.arff',
		   	   relation="sprint_data")
}
