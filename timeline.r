library(jsonlite)
library(logging)
library(plyr)
source('include/database.r')
source('include/log.r')
source('include/sprint_features.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

safe_unbox <- function(x) {
	if (is.data.frame(x) && nrow(x) == 0) {
		return(NA);
	}
	return(unbox(x));
}

conn <- connect()

items <- load_queries('sprint_events.yml', 'sprint_definitions.yml')

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

exportFeatures <- function(exclude) {
	result <- get_sprint_features(conn, exclude)
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
	names(project_data) <- projects$name
	write(toJSON(project_data), file="output/features.json")
}

# Export data to separate per-sprint files.
exportSplitData <- function(data, item) {
	project_data <- lapply(as.list(1:dim(projects)[1]), function(project) {
		project_name <- projects[project,'name']
		project_id <- projects[project,'project_id']

		sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id FROM gros.sprint WHERE sprint.project_id =', project_id))
		if (nrow(sprints) == 0) {
			return(sprints)
		}

		path <- paste("output", project_name, sep="/")
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
	names(project_data) <- projects$name 
	return(project_data)
}

# Export result of a type query to the correct JSON file(s).
exportData <- function(data, item) {
	if (isTRUE(item$split)) {
		return(exportSplitData(data, item))
	}
	project_data <- lapply(as.list(projects$name), function(project) {
		data[data$project_name == project,]
	})
	names(project_data) <- projects$name
	path <- paste("output", paste(item$type, "json", sep="."), sep="/")
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
	project_data <- exportData(result, item)
	have_data <- lapply(project_data, nrow) > 0
	projects_with_data <- modifyList(projects_with_data,
									 as.list(have_data)[have_data])

	minDate = min(result$date)
	maxDate = max(result$date, result$end_date)
	min_date[[item$type]] <- minDate
	max_date[[item$type]] <- maxDate

	type <- list(name=safe_unbox(item$type))
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

write(toJSON(total_data), file="output/data.json")
write(toJSON(types), file="output/types.json")
exportFeatures(get_arg('--exclude',
					   default='^new_developers$|^sprint_experience$'))
