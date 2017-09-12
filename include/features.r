# Utilities for retrieving sprint features.

source('include/database.r')
source('include/log.r')

get_features <- function(conn, exclude, items, data, colnames, join_cols) {
	for (item in items) {
		if (missing(exclude) || length(grep(exclude, item$table)) == 0) {
			loginfo('Executing query for table %s', item$table)
			time <- system.time(result <- dbGetQuery(conn, item$query))
			loginfo('Query for table %s took %f seconds', item$table,
					time['elapsed'])
			data <- merge(data, result, by=join_cols, all.x=T)
			if (!is.null(item$default)) {
				for (column in item$column) {
					data[is.na(data[[column]]),column] = item$default
				}
			}
			colnames <- c(colnames, item$column)
		}
	}
	list(data=data, colnames=colnames, items=items)
}

get_sprint_features <- function(conn, exclude, variables) {
	sprint_data <- dbGetQuery(conn,
						  	  'SELECT sprint.project_id, sprint.sprint_id
						  	  FROM gros.sprint
						  	  ORDER BY sprint.project_id, sprint.start_date'
						  	  )

	items <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
						  variables)
	colnames <- c("project_id")
	join_cols <- c("project_id", "sprint_id")
	get_features(conn, exclude, items, sprint_data, colnames, join_cols)
}

get_project_features <- function(conn, exclude, variables) {
	data <- dbGetQuery(conn,
					   'SELECT project.project_id, project.name
			  	  		FROM gros.project
	  	  				ORDER BY project.name'
  						)

	items <- load_queries('project_features.yml', 'sprint_definitions.yml',
						  variables)
	colnames <- c()
	join_cols <- c("project_id")
	get_features(conn, exclude, items, data, colnames, join_cols)
}
