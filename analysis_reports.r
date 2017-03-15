# Analysis report of done/not done in-progress stories per (normalized) story point.

source('database.r')
source('log.r')

not_done_ratio <- function(item, result) {
	bins <- c(0.0, 0.5, 1, 2, 3, 5, 8, 13, 20, 40, 100, Inf)
	codes <- .bincode(result$story_points, bins, right=F,
					  include.lowest=T)

	logdebug('Story points: %s', result$story_points)
	logdebug('Bin codes: %s', codes)

	num_not_done <- tapply(result$num_not_done, bins[codes],
						   na.rm=T, FUN=sum)
	num_done <- tapply(result$num_done, bins[codes],
					   na.rm=T, FUN=sum)
	ratio <- num_not_done/(num_not_done + num_done)*100

	logdebug('Summed not done:\n%s', log_format(num_not_done))
	logdebug('Summed done:\n%s', log_format(num_done))
	logdebug('Done/not-done ratio per story:\n%s', log_format(ratio))

	export_file <- paste("output", paste(item$table, "csv", sep="."), sep="/")
	write.table(as.table(ratio), file=export_file, row.names=F, sep=",",
				col.names=c('story points', 'not done ratio'))
	loginfo("Wrote report to %s", item$table)
}

reports <- list(not_done_ratio=not_done_ratio,
				not_done_ratio_log=not_done_ratio)

conn <- connect()
items <- load_queries('analysis_reports.yml', 'sprint_definitions.yml',
					  list(sprint_points_normalization=1,
						   category_conditions=''))

for (item in items) {
	loginfo('Executing query for report %s', item$table)
	loginfo('Query: %s', item$query)
	result <- dbGetQuery(conn, item$query)
	reports[[item$table]](item, result)
}
