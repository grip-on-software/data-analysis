# Analysis reports.

source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/analysis_reports.r')

conn <- connect()

projects_list <- get_arg('--projects', default='')
projects <- as.vector(as.numeric(unlist(strsplit(projects_list, ','))))

interval <- get_arg('--interval', default='')

report <- get_arg('--report', default='.*')

run_reports <- function(definitions) {
	items <- get_analysis_reports(definitions)

	for (item in items) {
		if (length(grep(report, item$table)) > 0) {
			loginfo('Executing query for report %s', item$table)
			result <- dbGetQuery(conn, item$query)
			item$report(item, result)
		}
	}
}

if (interval != '') {
	# Always run the full report, this will also empty the output directory
	run_reports(list(id='all'))

	start_date <- start_date <- dbGetQuery(conn, 'SELECT MIN(updated) AS start_date FROM gros.issue')[[1]]
	intervals <- seq(as.POSIXct(start_date), Sys.time(), by=interval)
	loginfo(intervals)
	write(toJSON(as.numeric(intervals)),
		  file=paste("output", "intervals.json", sep="/"))
	rollapply(intervals, 2, function(range) {
		run_reports(list(id=paste('interval', as.numeric(range[1]), sep='-'),
						 interval_condition=paste('WHERE ${field} BETWEEN ',
												  'epoch(', as.numeric(range[1]), ') AND ',
												  'epoch(', as.numeric(range[2]), ')', sep='')))
	})
} else if (identical(projects, numeric(0))) {
	run_reports(list(id='all'))
} else {
	for (project in projects) {
		run_reports(list(id=project,
						 category_conditions=paste('AND project_id =', project)))
		run_reports(list(id=paste('not', project, sep='-'),
						 category_conditions=paste('AND project_id <>', project)))
	}
}
