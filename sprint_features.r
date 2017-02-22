# R script that extracts features regarding sprints from the database.

library(foreign) # For write.arff
library(yaml)
source('database.r')

conn <- connect()

load_queries <- function(specification_file) {
	data <- yaml.load_file(specification_file)
	lapply(data$files, function(item) {
		path <- paste(data$path, item$filename, sep="/")
		item$query = readChar(path, file.size(path))
		item
	})
}

sprint_data <- dbGetQuery(conn,
						  'SELECT sprint.project_id, sprint.sprint_id
						  FROM gros.sprint'
						  )

items <- load_queries('sprint_features.yml')
colnames <- c("project_id")
join_cols <- c("project_id", "sprint_id")
for (item in items) {
	result <- dbGetQuery(conn, item$query)
	sprint_data <- merge(sprint_data, result, by=join_cols, all.x=T)
	if (!is.null(item$default)) {
		sprint_data[is.na(sprint_data[[item$column]]),item$column] = item$default
	}
	colnames <- c(colnames, item$column)
}

write.arff(sprint_data[,colnames], file='output/sprint_features.arff',
		   relation="sprint_data")
