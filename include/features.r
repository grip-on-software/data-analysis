# Utilities for retrieving sprint features.

library(jsonlite)
library(yaml)
source('include/database.r')
source('include/log.r')
source('include/project.r')

safe_unbox <- function(x) {
	if (is.vector(x) && length(x) > 1) {
		return(x)
	}
	if (is.vector(x) && length(x) == 0) {
		return(NA)
	}
	if (is.data.frame(x) && nrow(x) == 0) {
		return(NA)
	}
	if (is.list(x)) {
		return(sapply(x, safe_unbox, simplify=F))
	}
	return(unbox(x))
}

get_locales <- function(items) {
	locales <- list()
	for (type in names(items)) {
		item <- items[[type]]
		for (code in names(item)) {
			if (!(code %in% names(locales))) {
				locales[[code]] <- list()
			}
			locales[[code]][[type]] <- safe_unbox(item[[code]])
		}
	}
	return(locales)
}

get_feature_locales <- function(items, field='descriptions') {
	locales <- list()
	for (item in items) {
		for (code in names(item[[field]])) {
			if (!(code %in% names(locales))) {
				locales[[code]] <- list()
			}
			locales[[code]] <- c(locales[[code]],
								 mapply(function(column, description) {
									safe_unbox(description)
								 }, item$column, item[[field]][[code]],
								 SIMPLIFY=F))
		}
	}
	return(locales)
}

get_features <- function(conn, exclude, items, data, colnames, join_cols) {
	for (item in items) {
		if (missing(exclude) || length(grep(exclude, item$table)) == 0) {
			if (!is.null(item$result)) {
				result <- item$result
			}
			else {
				loginfo('Executing query for table %s', item$table)
				time <- system.time(result <- dbGetQuery(conn, item$query))
				loginfo('Query for table %s took %f seconds', item$table,
						time['elapsed'])
			}
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

get_sprint_conditions <- function(latest_date='', core=F, sprint_days=NA, sprint_patch=NA) {
	conditions <- list()
	if (!missing(latest_date) && latest_date != '') {
		conditions <- c(conditions,
						paste('sprint.start_date <= CAST(\'',
							  latest_date, '\' AS TIMESTAMP)', sep=''))
	}
	if (core) {
		conditions <- c(conditions, 'COALESCE(is_support_team, false) = false',
						'main_project IS NULL')
	}
	if (!is.na(sprint_days)) {
		conditions <- c(conditions, "${sprint_close} - sprint.start_date > interval '${sprint_days}' day")
	}
	if (!is.na(sprint_patch)) {
		conditions <- c(conditions, ifelse(sprint_patch, '${sprint_patch}',
										   'NOT (${sprint_patch})'))
	}
	return(conditions)
}

get_sprint_features <- function(conn, exclude, variables, latest_date, core=F, metrics=F, sprint_days=NA, sprint_patch=NA) {
	conditions <- get_sprint_conditions(latest_date, core, sprint_days, sprint_patch)
	if (length(conditions) != 0) {
		where_clause <- paste('WHERE', paste(conditions, collapse=' AND '))
		sprint_conditions <- paste('AND', paste(conditions, collapse=' AND '))
	}
	else {
		where_clause <- ''
		sprint_conditions <- ''
	}
	patterns <- load_definitions('sprint_definitions.yml',
								 list(sprint_days=sprint_days))
	sprint_query <- load_query(list(query=paste('SELECT sprint.project_id, sprint.sprint_id
									 			FROM gros.sprint
									 			JOIN gros.project
									 			ON project.project_id = sprint.project_id',
												where_clause, 'ORDER BY
									 			sprint.project_id, sprint.start_date')),
							   patterns)

	sprint_data <- dbGetQuery(conn, sprint_query$query)

	items <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
						  c(variables,
						  	list(sprint_conditions=str_interp(sprint_conditions,
															  patterns))))
	colnames <- c("project_id")
	join_cols <- c("project_id", "sprint_id")
	metric_cols <- c("project_id", "sprint_id", "value")

	if (metrics) {
		loginfo('Executing query for metric features')
		metric_data <- dbGetQuery(conn,
								  'SELECT metric.base_name, project_id,
								   sprint_id, AVG(value) AS value
								   FROM gros.metric_value
								   JOIN gros.metric
								   ON metric_value.metric_id = metric.metric_id
								   WHERE metric.base_name IS NOT NULL
								   AND sprint_id <> 0 AND value <> -1
								   GROUP BY metric.base_name, project_id, sprint_id
								   ORDER BY metric.base_name, project_id, sprint_id')
		for (metric_group in split(metric_data, metric_data$base_name)) {
			name <- metric_group$base_name[1]
			result <- metric_group[metric_cols]
			names(result) <- c(join_cols, name) 
			items <- c(items,
					   list(list(table=name, column=name, result=result)))
		}
	}

	get_features(conn, exclude, items, sprint_data, colnames, join_cols)
}

get_recent_sprint_features <- function(conn, features, date, limit=5, closed=T, sprint_meta=c(), sprint_conditions='') {
	patterns <- load_definitions('sprint_definitions.yml')
	projects <- get_recent_projects(conn, date)
	if (closed) {
		sprint_conditions <- paste(sprint_conditions,
								   'AND ${sprint_close} < CURRENT_TIMESTAMP()')
	}
	query = 'SELECT sprint.project_id, project.name AS project_name,
			project.quality_display_name,
			sprint.sprint_id, sprint.name AS sprint_name,
			sprint.start_date, ${sprint_close} AS close_date,
			sprint.board_id
			FROM gros.sprint
			JOIN gros.project
			ON project.project_id = sprint.project_id
			WHERE sprint.project_id = ${project_id}
			${sprint_conditions}
			ORDER BY sprint.project_id, sprint.start_date DESC
			LIMIT ${limit}'
	sprint_data <- data.frame()
	conditions <- str_interp(sprint_conditions, patterns)
	variables <- c(patterns, list(sprint_conditions=conditions, limit=limit))
	for (project in projects$project_id) {
		item <- load_query(list(query=query),
						   c(variables, list(project_id=project)))
		sprint_data <- rbind(sprint_data, dbGetQuery(conn, item$query))
	}
	sprint_data$start_date <- as.POSIXct(sprint_data$start_date)
	sprint_data$close_date <- as.POSIXct(sprint_data$close_date)

	data <- yaml.load_file('sprint_features.yml')
	items <- list()
	for (item in data$files) {
		if (length(item$column) == 1 && item$column %in% features) {
			items <- c(items, list(load_query(item, variables, data$path)))
		}
	}

	colnames <- c("project_name", "quality_display_name", sprint_meta)
	join_cols <- c("project_id", "sprint_id")
	get_features(conn, '^$', items, sprint_data, colnames, join_cols)
}

get_project_features <- function(conn, exclude, variables, core=F) {
	if (core) {
		data <- get_core_projects(conn, by='name')
	}
	else {
		data <- get_projects(conn, by='name')
	}

	items <- load_queries('project_features.yml', 'sprint_definitions.yml',
					  	  variables)
	colnames <- c()
	join_cols <- c("project_id")
	get_features(conn, exclude, items, data, colnames, join_cols)
}

write_feature_metadata <- function(projects, specifications, output_directory) {
	write(toJSON(get_feature_locales(specifications$files)),
	  	  file=paste(output_directory, "descriptions.json", sep="/"))
	write(toJSON(get_feature_locales(specifications$files, 'units')),
	  	  file=paste(output_directory, "units.json", sep="/"))
	write(toJSON(get_feature_locales(specifications$files, 'short_units')),
	  	  file=paste(output_directory, "short_units.json", sep="/"))
	write(toJSON(get_feature_locales(specifications$files, 'tags')),
	  	  file=paste(output_directory, "tags.json", sep="/"))
	write(toJSON(get_locales(yaml.load_file("source_types.yml"))),
	  	  file=paste(output_directory, "sources.json", sep="/"))
	write(toJSON(projects, auto_unbox=T),
	  	  file=paste(output_directory, "projects.json", sep="/"))
}
