# Analysis report of done/not done in-progress stories per (normalized) story point.

source('database.r')
source('log.r')

reports <- list(not_done_ratio=function(result) {
					bins <- c(0.0, 0.5, 1, 2, 3, 5, 8, 13, 20, 40, 100, Inf)
					codes <- .bincode(result$story_points, bins, right=F,
									  include.lowest=T)
					print(result$story_points)
					print(codes)
					num_not_done <- tapply(result$num_not_done, bins[codes],
										   na.rm=T, FUN=sum)
					num_done <- tapply(result$num_done, bins[codes],
									   na.rm=T, FUN=sum)
					print(num_not_done)
					print(num_done)
					print(num_not_done/(num_not_done + num_done)*100)
				})

conn <- connect()
items <- load_queries('analysis_reports.yml', 'sprint_definitions.yml',
					  list(sprint_points_normalization=1,
						   category_conditions=''))

for (item in items) {
	loginfo('Executing query for report %s', item$table)
	loginfo('Query: %s', item$query)
	result <- dbGetQuery(conn, item$query)
	reports[[item$table]](result)
}
