library(jsonlite)
library(logging)
library(plyr)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/features.r')
source('include/project.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

conn <- connect()

projects <- get_core_projects(conn)
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids = '1'
}
output_directory <- get_arg('--output', default='output')

variables <- list(project_ids=project_ids)
items <- load_queries('sprint_events.yml', 'sprint_definitions.yml', variables)

exportFeatures <- function(exclude, output_directory) {
	result <- get_sprint_features(conn, exclude, variables)
	data <- result$data
	colnames <- result$colnames
	project_data <- lapply(as.list(projects$project_id), function(project) {
		project_id <- projects[project,'project_id']
		sprint_data <- data[data$project_id == project,c('sprint_id', colnames)]
		result <- lapply(as.list(1:dim(sprint_data)[1]), function(i) {
			safe_unbox(sprint_data[i,colnames])
		})
		names(result) <- sprint_data$sprint_id
		return(result)
	})
	if (project_ids != '1') {
		names(project_data) <- projects$name
	}
	else {
		names(project_data) <- paste('Proj', projects$project_id, sep='')
	}
	write(toJSON(get_feature_locales(result$items)),
		  file=paste(output_directory, "locales.json", sep="/"))
	write(toJSON(project_data),
		  file=paste(output_directory, "features.json", sep="/"))
}

# Export data to separate per-sprint files.
exportSplitData <- function(data, item, output_directory) {
	project_data <- lapply(as.list(1:dim(projects)[1]), function(project) {
		project_id <- projects[project,'project_id']
		if (project_ids != '1') {
			project_name <- projects[project,'name']
		}
		else {
			project_name <- paste('Proj', project_id, sep='')
		}

		sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id FROM gros.sprint WHERE sprint.project_id =', project_id))
		if (nrow(sprints) == 0) {
			return(sprints)
		}

		path <- paste(output_directory, project_name, sep="/")
		if (!dir.exists(path)) {
			dir.create(path)
		}

		for (sprint_id in sprints$sprint_id) {
			sprint_split_data <- data[data$project_name == project_name & data$sprint_id == sprint_id,]

			filename = paste(path, paste(item$type, sprint_id, "json", sep="."),
							 sep="/")
			write(toJSON(sprint_split_data), file=filename)
		}
		return(sprints)
	})
	if (project_ids != '1') {
		names(project_data) <- projects$name
	}
	else {
		names(project_data) <- paste('Proj', projects$project_id, sep='')
	}
	return(project_data)
}

# Export result of a type query to the correct JSON file(s).
exportData <- function(data, item, output_directory) {
	if (isTRUE(item$split)) {
		return(exportSplitData(data, item, output_directory))
	}
	if (project_ids != '1') {
		project_names <- as.list(projects$name)
	}
	else {
		project_names <- paste('Proj', as.list(projects$project_id), sep='')
	}
	project_data <- lapply(project_names, function(project) {
		data[data$project_name == project,]
	})
	if (project_ids != '1') {
		names(project_data) <- projects$name
	}
	else {
		names(project_data) <- paste('Proj', projects$project_id, sep='')
	}
	path <- paste(output_directory, paste(item$type, "json", sep="."), sep="/")
	write(toJSON(project_data), file=path)
	return(project_data)
}

# Perform all queries and extract the extrema data.
min_date <- list()
max_date <- list()
types <- list()
projects_with_data <- list()
for (item in items) {
	loginfo('Executing query for type %s', item$type)
	time <- system.time(result <- dbGetQuery(conn, item$query))
	loginfo('Query for type %s took %f seconds', item$type, time['elapsed'])
	result$date = dateFormat(result$date)
	result$type = item$type
	if ("end_date" %in% result) {
		result$end_date = dateFormat(result$end_date)
	}
	project_data <- exportData(result, item, output_directory)
	have_data <- lapply(project_data, nrow) > 0
	projects_with_data <- modifyList(projects_with_data,
									 as.list(have_data)[have_data])

	minDate = min(result$date)
	maxDate = max(result$date, result$end_date)
	min_date[[item$type]] <- minDate
	max_date[[item$type]] <- maxDate

	type <- list(name=safe_unbox(item$type),
				 locales=safe_unbox(item$descriptions))
	if (!is.null(item$display)) {
		type$enabled = safe_unbox(item$display)
	}
	if (!is.null(item$split)) {
		type$subchart = safe_unbox(item$split)
	}
	types <- c(types, list(type))
}

total_data = list(min_date=safe_unbox(min(unlist(min_date))),
				  max_date=safe_unbox(max(unlist(max_date))),
				  update_date=safe_unbox(dateFormat(Sys.time())),
				  projects=names(projects_with_data))

write(toJSON(total_data), file=paste(output_directory, "data.json", sep="/"))
write(toJSON(types), file=paste(output_directory, "types.json", sep="/"))
exportFeatures(get_arg('--exclude', default='^$'), output_directory)
