# Utilities for retrieving sprint features.

source('log.r')

get_sprint_features <- function(conn) {
	sprint_data <- dbGetQuery(conn,
						  	  'SELECT sprint.project_id, sprint.sprint_id
						  	  FROM gros.sprint
						  	  ORDER BY sprint.project_id, sprint.start_date'
						  	  )

	items <- load_queries('sprint_features.yml')
	colnames <- c("project_id")
	join_cols <- c("project_id", "sprint_id")
	for (item in items) {
		loginfo('Executing query for table %s', item$table)
		result <- dbGetQuery(conn, item$query)
		sprint_data <- merge(sprint_data, result, by=join_cols, all.x=T)
		if (!is.null(item$default)) {
			for (column in item$column) {
				sprint_data[is.na(sprint_data[[column]]),column] = item$default
			}
		}
		colnames <- c(colnames, item$column)
	}
	list(data=sprint_data, colnames=colnames)
}
