# Analysis reports.

source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/analysis_reports.r')

conn <- connect()

projects_list <- get_arg('--projects', default='')
projects <- as.vector(as.numeric(unlist(strsplit(projects_list, ','))))

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

if (identical(projects, numeric(0))) {
	run_reports(list(id='all'))
} else {
	for (project in projects) {
		run_reports(list(id=project,
						 category_conditions=paste('AND project_id =', project)))
		run_reports(list(id=paste('not', project, sep='-'),
						 category_conditions=paste('AND project_id <>', project)))
	}
}
