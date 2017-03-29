# Analysis report of done/not done in-progress stories per (normalized) story point.

library(yaml)
library(ggplot2)
source('include/args.r')
source('include/database.r')
source('include/log.r')

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
	aspect_ratio = 1/1.6
	for (project in levels(factor(result$project_id))) {
		for (sprint in levels(factor(result[result$project_id == project,'sprint_id']))) {
			sprint_data = result[result$project_id == project & result$sprint_id == sprint,c('story_points', 'close_date')]
			start_points <- sprint_data[1,'story_points']
			end_time <- sprint_data[sprint_data$story_points == 0,'close_date']
			if (!is.na(start_points) && !identical(end_time, character(0))) {
				export_file <- paste("output",
									 paste(paste(item$table, project, sprint,
									 			 sep="-"),
									 	   "pdf", sep="."),
									 sep="/")

				points <- cumsum(sprint_data$story_points)
				date <- as.Date(sprint_data$close_date, '%Y-%m-%d')
				end_date <- as.Date(end_time, '%Y-%m-%d')
				data <- cbind(as.data.frame(sprint_data$close_date),
							  as.data.frame(points))
				print(data)
				plot <- ggplot(data, aes(x=date, y=points, group=1)) +
					geom_point() + geom_line() +
					geom_segment(aes(x=date[1], y=start_points,
									 xend=end_date, yend=0), colour='blue') +
					geom_vline(colour='red', xintercept=as.numeric(end_date)) +
					coord_equal(ratio=aspect_ratio) +
					theme(aspect.ratio=aspect_ratio)
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

report = get_arg('--report', default='.*')

for (item in items) {
	if (length(grep(report, item$table)) > 0) {
		loginfo('Executing query for report %s', item$table)
		loginfo('Query: %s', item$query)
		result <- dbGetQuery(conn, item$query)
		reports[[item$table]](item, result)
	}
}
