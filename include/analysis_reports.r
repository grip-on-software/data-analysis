# Analysis reports.

library(yaml)
library(ggplot2)
source('include/log.r')

not_done_ratio <- function(item, result) {
	bins <- c(0.0, 0.5, 1, 2, 3, 5, 8, 13, 20, 40, 100, Inf)
	codes <- .bincode(result$story_points, bins, right=F,
					  include.lowest=T)

	logdebug('Story points: %s', result$story_points)
	logdebug('Bin codes: %s', codes)

	all_not_done <- tapply(result$num_not_done, bins[codes],
						   na.rm=T, FUN=sum)
	all_done <- tapply(result$num_done, bins[codes],
					   na.rm=T, FUN=sum)

	num_not_done <- all_not_done[all_done + all_not_done > 1]
	num_done <- all_done[all_done + all_not_done > 1]

	ratio <- num_not_done/(num_not_done + num_done)*100
	done_ratio <- num_done/(num_not_done + num_done)*100

	logdebug('Summed not done:\n%s', log_format(num_not_done))
	logdebug('Summed done:\n%s', log_format(num_done))
	logdebug('Done/not-done ratio per story:\n%s', log_format(ratio))

	output <- data.frame(story_points=row.names(ratio),
						 num_not_done=num_not_done, num_done=num_done,
						 ratio=ratio)

	filename <- paste(item$table, item$patterns[['id']], sep='-')
	export_file <- paste("output", paste(filename, 'csv', sep='.'), sep="/")
	write.table(output, file=export_file, row.names=F, sep=",")
	loginfo("Wrote report to %s", export_file)

	num_points = length(output$story_points)

	plot_data <- data.frame(x=output$story_points,
							y=ratio,
							group=rep("not done", num_points),
							label=paste(num_not_done, num_done, sep='\n'))
	scale <- log10(num_done + num_not_done) / log10(max(num_done + num_not_done))
	ggplot(data = plot_data, aes(x=plot_data$x, y=plot_data$y,
								 fill=plot_data$group,
								 label=plot_data$label)) +
		scale_x_discrete(limits=output$story_points) +
		geom_bar(stat="identity", position="dodge", width=scale) +
		geom_text(position=position_dodge(0.8)) +
		labs(title=paste("Story points not done ratio (",
						 item$patterns[['id']], ")", sep=''),
			 x="Story points", y="Ratio (% not done)") +
		theme(plot.title = element_text(hjust = 0.5), legend.position="none")

	plot_file <- paste("output", paste(filename, 'png', sep='.'), sep='/')
	ggsave(plot_file)
	loginfo("Wrote plot to %s", plot_file)
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

				ideal_line = function(x) {
					-start_points/(as.numeric(end_date) - as.numeric(date[1])) * (as.numeric(x) - as.numeric(date[1])) + start_points
				}
				line_points = ideal_line(date)
				over_under = points > line_points
				# Frequency of events that are above the ideal line
				num_over = sum(over_under)/length(points)
				changes = diff(over_under)
				# Frequency of crossing the ideal line
				num_changes = sum(abs(changes)/length(points))
				# Events after which the progress line crosses ideal line
				indexes = which(changes != 0)
				# use for intersection detection and area calculation
				low_indexes = which(over_under == F)
				high_indexes = which(over_under == T)
				low_distances = line_points[low_indexes] - points[low_indexes]
				high_distances = points[high_indexes] - line_points[high_indexes]
				print(sum(low_distances))
				print(sum(high_distances))

				# More details
				print(line_points)
				print(over_under)
				print(sum(over_under))
				print(data)
				# Output plot
				plot <- ggplot(data, aes(x=date, y=points, group=1)) +
					geom_point() + geom_line() +
					geom_segment(aes(x=date[1], y=start_points,
									 xend=end_date, yend=0), colour='blue') +
					geom_vline(colour='red', xintercept=as.numeric(end_date)) +
					coord_equal(ratio=aspect_ratio) +
					theme(aspect.ratio=aspect_ratio)
				ggsave(export_file)
				loginfo("Wrote plot to %s", export_file)
			}
		}
	}
}

get_analysis_reports <- function(analysis_variables) {
	reports <- list(not_done_ratio=not_done_ratio,
					not_done_ratio_log=not_done_ratio,
					sprint_burndown=sprint_burndown)
	definitions <- yaml.load_file('analysis_definitions.yml')
	analysis_definitions <- modifyList(lapply(definitions$fields,
							   	              function(define) { define$field }),
									   analysis_variables) 
	print(analysis_definitions)
	items <- load_queries('analysis_reports.yml', 'sprint_definitions.yml',
					  	  analysis_definitions)
	lapply(items, function(item) {
		item$report <- reports[[item$table]]
		return(item)
	})
}