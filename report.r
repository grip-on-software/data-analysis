# Analysis reports.

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/analysis_reports.r')

conn <- connect()

projects_list <- get_arg('--projects', default='')
invert <- get_arg('--invert', default=F)

interval <- get_arg('--interval', default='')

report <- get_arg('--report', default='.*')

output_directory <- get_arg('--output', default='output')

project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids = '1'
}

run_reports <- function(definitions) {
	items <- get_analysis_reports(definitions)

	for (item in items) {
		if (length(grep(report, item$table)) > 0) {
			loginfo('Executing query for report %s', item$table)
			time <- system.time(result <- dbGetQuery(conn, item$query))
			loginfo('Query for report %s took %f seconds', item$table,
					time['elapsed'])
			time <- system.time(item$report(item, result, output_directory))
			loginfo('Generation of report %s took %f seconds', item$table,
					time['elapsed'])
		}
	}
}

if (interval != '') {
	# Always run the full report, this will also empty the output directory
	run_reports(list(id='all', project_ids=project_ids))

	start_date <- dbGetQuery(conn, 'SELECT MIN(updated) AS start_date FROM gros.issue WHERE assignee IS NOT NULL')[[1]]
	intervals <- seq(as.POSIXct(start_date), Sys.time(), by=interval)
	loginfo(intervals)
	output_directory <- get_arg('--output', default='output')
	write(toJSON(head(as.numeric(intervals), n=-1)),
		  file=paste(output_directory, "intervals.json", sep="/"))
	rollapply(intervals, 2, function(range) {
		run_reports(list(id=paste('interval', as.numeric(range[1]), sep='-'),
						 project_ids=project_ids,
						 interval_condition=paste('WHERE ${field} BETWEEN ',
												  'epoch(', as.numeric(range[1]), ') AND ',
												  'epoch(', as.numeric(range[2]), ')', sep='')))
	})
} else if (projects_list == '') {
	run_reports(list(id='all', project_ids=project_ids))
} else {
	if (projects_list == 'each') {
		projects <- get_projects(conn)$project_id
	}
	else {
		projects <- as.vector(as.numeric(unlist(strsplit(projects_list, ','))))
	}
	for (project in projects) {
		run_reports(list(id=project,
						 project_ids=project_ids,
						 category_conditions=paste('AND project_id =', project)))
		if (invert) {
			run_reports(list(id=paste('not', project, sep='-'),
						 	 project_ids=project_ids,
						 	 category_conditions=paste('AND project_id <>', project)))
		}
	}
	write(toJSON(projects),
		  file=paste(output_directory, 'report_projects.json', sep='/'))
}
