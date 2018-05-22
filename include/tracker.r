# Functions for obtaining update tracker age.

library(jsonlite)
library(yaml)
source('include/database.r')

trackers <- yaml.load_file('trackers.yml')

get_source_dates <- function(tracker, contents) {
	if ("format" %in% names(tracker)) {
		return(list("source"=as.POSIXct(strptime(contents, tracker$format))))
	}
	else if ("json" %in% names(tracker)) {
		items <- fromJSON(gsub("\\\\", "", contents))
		if (tracker$json == "object") {
			return(lapply(items, as.POSIXct))
		}
		else {
			stop(paste("I don't know what to do for tracker",
					   tracker$file, "with JSON parse type",
					   tracker$json))
		}
	}
}

get_tracker_dates <- function(conn, project_id, sources='all', with_sources=F) {
	filenames <- unlist(lapply(names(trackers), function(group) {
		if (sources == 'all' || group %in% sources) {
			lapply(trackers[[group]], function(tracker) { tracker$file })
		}
	}))
	query <- paste('SELECT filename, contents, update_date',
				   'FROM gros.update_tracker',
				   'WHERE project_id =', project_id,
				   'AND filename IN (',
				   paste(dbQuoteString(conn, filenames), collapse=','), ')')

	files <- dbGetQuery(conn, query)
	result <- list()
	for (group in names(trackers)) {
		if (sources == 'all' || group %in% sources) {
			dates <- list()
			for (tracker in trackers[[group]]) {
				if (tracker$file %in% files$filename) {
					update_date <- files[files$filename == tracker$file,'update_date']
					dates[[tracker$file]] <- as.POSIXct(update_date)
					if (with_sources) {
						contents <- files[files$filename == tracker$file,'contents']
						dates <- c(dates, get_source_dates(tracker, contents))
					}
				}
			}
			result[[group]] <- dates
		}
	}
	return(result)
}
