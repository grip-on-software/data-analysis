# Analysis reports.

source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/analysis_reports.r')

conn <- connect()
items <- get_analysis_reports(conn)

report = get_arg('--report', default='.*')

for (item in items) {
	if (length(grep(report, item$table)) > 0) {
		loginfo('Executing query for report %s', item$table)
		result <- dbGetQuery(conn, item$query)
		reports[[item$table]](item, result)
	}
}
