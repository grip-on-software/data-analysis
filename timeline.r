library(jsonlite)
library(plyr)
source('database.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

conn <- connect()

items <- load_queries('sprint_events.yml')

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

# Export data to separate per-sprint files.
exportSplitData <- function(data, item) {
	lapply(as.list(1:dim(projects)[1]), function(project) {
		project_name <- projects[project,'name']
		project_id <- projects[project,'project_id']

		path <- paste("output", project_name, sep="/")
		if (!dir.exists(path)) {
			dir.create(path)
		}

		sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id FROM gros.sprint WHERE sprint.project_id =', project_id))
		for (sprint_id in sprints$sprint_id) {
			sprint_split_data <- data[data$project_name == project_name & data$sprint_id == sprint_id,]

			filename = paste(path, paste(item$type, sprint_id, "json", sep="."),
							 sep="/")
			write(toJSON(sprint_split_data), file=filename)
		}
		return(data)
	})
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
	return(data)
}

# Perform all queries and extract the extrema data.
min_date <- list()
max_date <- list()
for (item in items) {
	result <- dbGetQuery(conn, item$query)
	result$date = dateFormat(result$date)
	result$type = item$type
	if ("end_date" %in% result) {
		result$end_date = dateFormat(result$end_date)
	}
	exportData(result, item)

	minDate = min(result$date)
	maxDate = max(result$date, result$end_date)
	min_date[[item$type]] <- minDate
	max_date[[item$type]] <- maxDate
}

total_data = list(min_date=min(unlist(min_date)),
				  max_date=max(unlist(max_date)),
				  update_date=dateFormat(Sys.time()),
				  projects=projects$name)

write(toJSON(total_data), file="output/data.json")
