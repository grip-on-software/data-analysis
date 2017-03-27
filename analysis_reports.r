# Analysis report of done/not done in-progress stories per (normalized) story point.

library(yaml)
library(ggplot2)
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

	output <- data.frame(story_points=row.names(ratio),
						 num_not_done=num_not_done, num_done=num_done,
						 ratio=ratio)

	export_file <- paste("output", paste(item$table, "csv", sep="."), sep="/")
	write.table(output, file=export_file, row.names=F, sep=",")
	loginfo("Wrote report to %s", item$table)
}

sprint_burndown <- function(item, result) {
	#dev.new()
	for (project in levels(factor(result$project_id))) {
		for (sprint in levels(factor(result[result$project_id == project,'sprint_id']))) {
			sprint_data = result[result$project_id == project & result$sprint_id == sprint,c('story_points', 'close_date')]
			if (!is.na(sprint_data[1,'story_points'])) {
				export_file <- paste("output",
									 paste(paste(item$table, project, sprint,
									 			 sep="-"),
									 	   "pdf", sep="."),
									 sep="/")

				points = cumsum(sprint_data$story_points)
				date <- as.Date(sprint_data$close_date, '%Y-%m-%d')
				end_date <- sprint_data[sprint_data$story_points == 0,'close_date']
				data <- cbind(as.data.frame(sprint_data$close_date),
							  as.data.frame(points))
				print(data)
				plot <- ggplot(data, aes(x=date, y=points, group=1)) + geom_line() + geom_vline(colour='red', xintercept=end_date)
				ggsave(export_file)
			}
		}
	}
}

reports <- list(not_done_ratio=not_done_ratio,
				not_done_ratio_log=not_done_ratio,
				sprint_burndown=sprint_burndown)

conn <- connect()
definitions <- yaml.load_file('analysis_definitions.yml')
analysis_definitions <- lapply(definitions$fields,
							   function(define) { define$field })
items <- load_queries('analysis_reports.yml', 'sprint_definitions.yml',
					  analysis_definitions)

for (item in items) {
	loginfo('Executing query for report %s', item$table)
	loginfo('Query: %s', item$query)
	result <- dbGetQuery(conn, item$query)
	reports[[item$table]](item, result)
}
