# Utilities for retrieving sprint features.

source('include/database.r')
source('include/log.r')
source('include/project.r')

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
					if (column %in% names(data)) {
						if (length(data[[column]]) == 0) {
							logwarn(paste('Column', column, 'is empty'))
						}
						else {
							data[is.na(data[[column]]),column] = item$default
						}
					}
					else {
						logwarn(paste('Column', column, 'could not be found'))
					}
				}
			}
			colnames <- c(colnames, item$column)
		}
	}
	list(data=data, colnames=colnames, items=items)
}

get_sprint_features <- function(conn, exclude, variables, latest_date) {
	if (!missing(latest_date) && latest_date != '') {
		condition <- paste('WHERE sprint.start_date <= CAST(\'',
						   latest_date, '\' AS TIMESTAMP)', sep='')
	}
	else {
		condition <- ''
	}
	sprint_data <- dbGetQuery(conn,
						  	  paste('SELECT sprint.project_id, sprint.sprint_id
									 FROM gros.sprint', condition, 'ORDER BY
									 sprint.project_id, sprint.start_date'))

	items <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
						  variables)
	colnames <- c("project_id")
	join_cols <- c("project_id", "sprint_id")
	get_features(conn, exclude, items, sprint_data, colnames, join_cols)
}

get_recent_sprint_features <- function(conn, features, variables, date, limit) {
	if (missing(limit)) {
		limit <- 5
	}
	patterns <- load_definitions('sprint_definitions.yml', variables)
	projects <- get_recent_projects(conn, date)
	query = 'SELECT sprint.project_id, project.name AS project_name,
			 sprint.sprint_id, sprint.name AS sprint_name, sprint.start_date
		  	 FROM gros.sprint
		  	 JOIN gros.project
		  	 ON project.project_id = sprint.project_id
		  	 WHERE sprint.project_id = ${project_id}
		  	 AND ${sprint_close} < CURRENT_TIMESTAMP()
			 AND sprint.name NOT LIKE \'Technical%\'
		  	 ORDER BY sprint.project_id, sprint.start_date DESC
		  	 LIMIT ${limit}'
	sprint_data <- data.frame()
	for (project in projects$project_id) {
		item <- load_query(list(query=query),
						   c(patterns, list(limit=limit, project_id=project)))
		sprint_data <- rbind(sprint_data, dbGetQuery(conn, item$query))
	}
	sprint_data$start_date <- as.POSIXct(sprint_data$start_date)

	data <- yaml.load_file('sprint_features.yml')
	items <- list()
	for (item in data$files) {
		if (length(item$column) == 1 && item$column %in% features) {
			items <- c(items, list(load_query(item, patterns, data$path)))
		}
	}

	colnames <- c("project_name", "sprint_name", "start_date")
	join_cols <- c("project_id", "sprint_id")
	get_features(conn, '^$', items, sprint_data, colnames, join_cols)
}

get_project_features <- function(conn, exclude, variables) {
	data <- get_projects(conn, by='name')

	items <- load_queries('project_features.yml', 'sprint_definitions.yml',
					  	  variables)
	colnames <- c()
	join_cols <- c("project_id")
	get_features(conn, exclude, items, data, colnames, join_cols)
}
