# Analysis reports.

library(yaml)
library(jsonlite)
library(ggplot2)
library(openssl)
library(padr)
library(stringr)
library(viridis)
library(zoo)
source('include/args.r')
source('include/log.r')
source('include/project.r')

not_done_ratio <- function(item, result, output_dir) {
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
	export_file <- paste(output_dir, paste(filename, 'csv', sep='.'), sep="/")
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

	plot_file <- paste(output_dir, paste(filename, 'png', sep='.'), sep='/')
	ggsave(plot_file)
	loginfo("Wrote plot to %s", plot_file)
}

sprint_burndown <- function(item, result, output_dir) {
	format <- get_arg('--format', default='pdf')
	projects <- get_projects(conn)

	baseDir <- paste(output_dir, item$table, sep="/")
	if (!dir.exists(baseDir)) {
		dir.create(baseDir)
	} else {
		loginfo("Emptying %s directory", baseDir)
		unlink(paste(baseDir, "/*", sep=""), recursive=TRUE)
	}
	aspect_ratio = 1/1.6
	for (project in levels(factor(result$project_id))) {
		for (sprint in levels(factor(result[result$project_id == project,'sprint_id']))) {
			if (item$patterns[['project_ids']] != '1') {
				project_name <- projects[projects$project_id == project, 'name']
			}
			else {
				project_name <- paste('Proj', project, sep='')
			}
			sprint_data <- result[result$project_id == project & result$sprint_id == sprint,c('story_points', 'close_date', 'key', 'event_type')]
			start_points <- sprint_data[1,'story_points']
			end_time <- sprint_data[sprint_data$event_type=='close','close_date']
			if (!is.na(start_points) && !identical(end_time, character(0))) {
				path <- paste(baseDir, project_name, sep="/")
				if (!dir.exists(path)) {
					dir.create(path)
				}

				export_file <- paste(path,
						paste(paste(item$table, sprint,
									sep="."),
							format, sep="."),
						sep="/")

				changes <- sprint_data$story_points
				# Set close date points change to 0.0
				changes[is.na(changes)] <- 0.0
				points <- cumsum(changes)
				date <- as.Date(sprint_data$close_date, '%Y-%m-%d')
				end_date <- as.Date(end_time, '%Y-%m-%d')
				type <- sprint_data$event_type
				data <- cbind(as.data.frame(sprint_data$close_date),
							  as.data.frame(points),
							  as.data.frame(sprint_data$event_type))

				max_points <- max(points) - sum(sprint_data[sprint_data$event_type=='scope_remove','story_points'])
				ideal_line = function(x) {
					-max_points/(as.numeric(end_date) - as.numeric(date[1])) * (as.numeric(x) - as.numeric(date[1])) + max_points
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
				# Output plot
				if (format == 'pdf') {
					plot <- ggplot(data, aes(x=date, y=points, group=1)) +
					geom_point(aes(colour=factor(type))) +
					geom_line() +
					geom_segment(aes(x=date[1], y=start_points,
									 xend=end_date, yend=0), colour='blue') +
					geom_vline(colour='red', xintercept=as.numeric(end_date)) +
					coord_equal(ratio=aspect_ratio) +
					theme(aspect.ratio=aspect_ratio)
					ggsave(export_file)
					loginfo("Wrote plot to %s", export_file)
				} else if (format == 'json') {
					names(data)[names(data)=='sprint_data$close_date'] <- 'date'
					names(data)[names(data)=='sprint_data$event_type'] <- 'type'

					write(toJSON(data),file=export_file)
					loginfo("Wrote data to %s", export_file)
				} else if (format == 'txt') {
					print(sum(low_distances))
					print(sum(high_distances))

					# More details
					print(line_points)
					print(over_under)
					print(sum(over_under))
					print(data)
				} else {
					loginfo("Not a supported format")
				}
			}
		}
	}
}

commit_volume <- function(item, result, output_dir) {
	projects <- get_repo_projects(conn)
	data <- lapply(as.list(projects$project_id), function(project_id) {
		commit_data <- result[result$project_id == project_id,c('commit_day','value')]
		names(commit_data)[names(commit_data)=='commit_day'] <- 'day'
		return(commit_data)
	})
	if (item$patterns[['project_ids']] != '1') {
		names(data) <- projects$name
	}
	else {
		names(data) <- paste('Proj', projects$project_id, sep='')
	}
	write(toJSON(data),
		  file=paste(output_dir, paste(item$table, "json", sep="."), sep="/"))
}

developers <- function(item, result, output_dir) {
	projects <- get_repo_projects(conn)
	data <- lapply(as.list(projects$project_id), function(project_id) {
		dev_data <- result[result$project_id == project_id,c('commit_date','value')]
		date_data <- data.frame(day=as.Date(dev_data$commit_date),
								value=as.numeric(dev_data$value))
		if (nrow(date_data) == 0) {
			return(date_data)
		}
		pad_data <- na.locf(pad(date_data, interval='day'))
		pad_data$value <- as.numeric(pad_data$value)
		return(pad_data)
	})
	if (item$patterns[['project_ids']] != '1') {
		names(data) <- projects$name
	}
	else {
		names(data) <- paste('Proj', projects$project_id, sep='')
	}
	write(toJSON(data),
		  file=paste(output_dir, paste(item$table, "json", sep="."), sep="/"))
}

to_map <- function(df) {
	map <- sapply(df$id, function(id) {
		return(df[df$id == id,2])
	}, simplify=F)
	names(map) <- df$id
	map[['0']] <- ''
	print(map)
	return(map)
}

story_flow <- function(item, result, output_dir) {
	states <- to_map(dbGetQuery(conn, 'SELECT id, name FROM gros.status'))
	resolutions <- to_map(dbGetQuery(conn,
									 'SELECT id, name FROM gros.resolution'))

	columns <- c('old_status','old_resolution','new_status','new_resolution')
	changes <- split(result, list(result$old_status, result$old_resolution, result$new_status, result$new_resolution), drop=T)
	nodes <- list()
	edges <- list()
	total_stories <- nrow(result)
	avg_volume <- total_stories / length(changes)
	colors <- list('1'='blue', # Open
				   '3'='yellow', # In Progress
				   '4'='gray', # Reopened
				   '5'='green', # Resolved
				   '6'='darkgreen', # Closed
				   '10004'='yellow', # Backlog Approved
				   '10005'='yellow', # Reviewed
				   '10006'='yellow') # In Review
	color_names <- list(blue='open', yellow='progress', gray='reopened',
						green='resolved', darkgreen='closed')
	# from max (bottom) to min (top)
	ranks <- list(darkgreen=c(), green=c(), gray=c(), yellow=c(), blue=c())
	max_time <- 150
	palette <- substr(viridis_pal(alpha=0, end=0.75, option="plasma")(max_time+1),
					  0, 7)

	for (change in changes) {
		old_status_id <- as.character(change[1,'old_status'])
		old_resolution_id <- as.character(change[1,'old_resolution'])
		new_status_id <- as.character(change[1,'new_status'])
		new_resolution_id <- as.character(change[1,'new_resolution'])

		old_status <- states[[old_status_id]]
		old_resolution <- resolutions[[old_resolution_id]]
		new_status <- states[[new_status_id]]
		new_resolution <- resolutions[[new_resolution_id]]

		if (is.null(old_status) || is.null(new_status)) {
			next
		}
		if (old_status_id == '1' && old_resolution != '') {
			next
		}

		volume <- nrow(change)
		loginfo('Volume: %d/%d', volume, total_stories)
		time_delta <- mean(as.numeric(difftime(change$new_date, change$earliest_date, units="days"))*7.0/5)

		old_name <- paste('"', paste(old_status, old_resolution, sep=" "),
						  '"', sep='')
		new_name <- paste('"', paste(new_status, new_resolution, sep=" "),
						  '"', sep='')

		old_attrs <- list(style='rounded', shape='box')
		new_attrs <- list(style='rounded', shape='box')
		if (old_status_id %in% names(colors)) {
			color <- colors[[old_status_id]]
			ranks[[color]] <- c(ranks[[color]], old_name)
			old_attrs$color <- color
		}
		if (new_status_id %in% names(colors)) {
			color <- colors[[new_status_id]]
			ranks[[color]] <- c(ranks[[color]], new_name)
			new_attrs$color <- color
		}

		nodes[[old_name]] <- old_attrs
		nodes[[new_name]] <- new_attrs

		edge_attrs <- list(label=paste('"', round(time_delta), ' days\\n',
									   volume, ' stories"', sep=""),
						   fontcolor=paste('"', palette[1+round(min(time_delta,max_time))], '"', sep=""),
						   penwidth=1+log(1+0.5*(volume - avg_volume)/avg_volume))

		edge <- paste(old_name, new_name, sep=" -> ")
		edges[[edge]] <- edge_attrs

		loginfo("Old status: %s Old resolution: %s New status: %s New resolution: %s Count: %s Average time: %s",
				old_status, old_resolution, new_status, new_resolution,
				volume, time_delta)
	}

	dot_attrs <- function(attrs) {
		return(paste("[",
					 paste(names(attrs), attrs, sep="=", collapse=","),
					 "];", sep=""))
	}

	dot_ranks <- function(rank, num) {
		if (length(rank) == 0) {
			return("")
		}
		type <- "same"
		if (num == 1) {
			type <- "max"
		}
		else if (num == length(ranks)) {
			type <- "min"
		}
		return(paste("{rank = ", type, ";",
					 paste(levels(factor(rank)), collapse="; "),
					 "}", sep=""))
	}

	dot <- c("digraph G {",
			 paste(names(edges), lapply(edges, dot_attrs)),
			 paste(names(nodes), lapply(nodes, dot_attrs)),
			 paste(mapply(dot_ranks, ranks, 1:length(ranks))),
			 "}")

	export_file <- paste(output_dir,
						 paste(paste(item$table, item$patterns[['name']],
						 			 sep="-"), "dot", sep="."), sep="/")
	writeLines(dot, export_file)
	loginfo("Exported graph to %s", export_file)

	# Write a Makefile
	writeLines(c(".PHONY: all",
				 "all: $(patsubst %.dot,%.png,$(wildcard *.dot))",
				 "%.png: %.dot",
				 "\tdot -Tpng $< -o $@"),
			   paste(output_dir, "Makefile", sep="/"))
	write(toJSON(palette),
		  file=paste(output_dir, "story_flow_palette.json", sep="/"))
	write(toJSON(color_names, auto_unbox=T),
		  file=paste(output_dir, "story_flow_states.json", sep="/"))
}

long_waiting_commits <- function(item, result, output_dir) {
	path <- paste(output_dir, item$table, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}
	projects <- get_repo_projects(conn)
	mapply(function(project_id, name) {
		project_data <- result[result$project_id == project_id,c('repo_name','url','environment_url','file','later_date','earlier_date')]

		if (item$patterns[['project_ids']] == '1') {
			name <- paste('Proj', project_id, sep='')
			project_data$repo_name <- unclass(sha256(project_data$repo_name))
			project_data$url <- NULL
			project_data$file <- unclass(sha256(project_data$file))
		}
		else if (is.null(project_data$url) || length(grep("^https?://", project_data$url)) == 0) {
			# Link to a webpage with relevant information about the source.
			project_data$url <- project_data$environment_url
		}
		project_data$environment_url <- NULL
		project_data$earlier_date <- as.POSIXct(project_data$earlier_date)
		project_data$later_date <- as.POSIXct(project_data$later_date)
		write(toJSON(project_data),
			  file=paste(path, paste(name, "json", sep="."), sep="/"))
	}, as.list(projects$project_id), as.list(projects$name))
}

project_members <- function(item, result, output_dir) {
	path <- paste(output_dir, item$table, sep="/")
	if (item$patterns[['id']] != 'all') {
		if (!dir.exists(path)) {
			dir.create(path)
		}
		filename <- paste(item$table, item$patterns[['id']], sep='-')
	} else {
		loginfo("Emptying %s directory", path)
		unlink(paste(path, "/*", sep=""))

		path <- output_dir # Put the full report in the base output directory
		filename <- item$table
	}
	write(toJSON(result),
		  file=paste(path, paste(filename, "json", sep="."), sep="/"))
}

project_backlog_burndown <- function(item, result, output_dir) {
	# Output plot
	format <- get_arg('--format', default='json')
	export_file = function(name, format) {
		paste(output_dir, paste(name, format, sep="."), sep="/")
	}
	if (format == 'pdf') {
		for (project in levels(factor(result$project_id))) {
			project_data <- result[result$project_id == project,]
			date <- as.Date(project_data$start_date, '%Y-%m-%d')
			epic_points <- project_data$num_epics * mean(na.omit(project_data$num_epic_points/project_data$num_epics))
			points <- project_data$num_points + (!is.na(epic_points) & epic_points)
			data <- cbind(as.data.frame(project_data$start_date),
						  as.data.frame(points))
			loginfo(date)
			loginfo(points)
			aspect_ratio = 1/1.6
			plot <- ggplot(data, aes(x=date, y=points, group=1)) +
				geom_point() + geom_line() +
				coord_equal(ratio=aspect_ratio) +
				theme(aspect.ratio=aspect_ratio)
			file <- export_file(paste(item$table, project, sep="-"), format)
			ggsave(file)
			loginfo("Wrote plot to %s", file)
		}
	} else if (format == 'json') {
		write(toJSON(result), file=export_file(item$table, format))
	}
}

bigboat_status <- function(item, result, output_dir) {
	path <- paste(output_dir, item$table, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}

	# Write the status field descriptions
	status <- yaml.load_file("bigboat_status.yml")
	write(toJSON(status$fields, auto_unbox=T),
		  file=paste(path, "fields.json", sep="/"))

	matches <- unlist(status$match)

	projects <- get_projects(conn)

	project_ids <- list()
	project_names <- list()

	for(project_id in as.list(projects$project_id)) {
		project_name <- projects[projects$project_id == project_id,'name']
		project_data <- result[result$project_id == project_id,c('name','checked_date','ok','value','max')]

		if (nrow(project_data) > 0) {
			project_data$name <- str_replace_all(project_data$name, matches)
			project_data$checked_date <- as.POSIXct(project_data$checked_date)
			if (item$patterns[['project_ids']] != '1') {
				name <- project_name
			}
			else {
				name <- paste('Proj', project_id, sep='')
			}
			project_ids <- c(project_ids, project_id)
			project_names <- c(project_names, name)

			write(toJSON(project_data[with(project_data,
										   order(name, checked_date)),]),
				file=paste(path, paste(name, "json", sep="."), sep="/"))
		}
		else {
			if (item$patterns[['project_ids']] != '1') {
				loginfo("No data for project %d", project_id)
			}
			else {
				loginfo("No data for %s", project_name)
			}
		}
	}

	write(toJSON(project_names, auto_unbox=T),
		file=paste(path, "projects.json", sep="/"))

	if (length(project_ids) > 0 && item$patterns[['project_ids']] != '1') {
		# Create list of source URLs
		urls <- dbGetQuery(conn, paste("SELECT project_id, url
										FROM gros.source_environment
										WHERE project_id IN (",
										paste(project_ids, collapse=","), ")
										AND source_type = 'bigboat'", sep=""))
		names(project_names) <- project_ids
		project_urls <- as.list(urls$url)
		names(project_urls) <- project_names[as.character(urls$project_id)]
	}
	else {
		project_urls <- list()
	}
	write(toJSON(project_urls, auto_unbox=T),
		  file=paste(path, "urls.json", sep="/"))
}

get_analysis_reports <- function(analysis_variables) {
	reports <- list(not_done_ratio=not_done_ratio,
					not_done_ratio_log=not_done_ratio,
					sprint_burndown=sprint_burndown,
					commit_volume=commit_volume,
					developers=developers,
					story_flow=story_flow,
					long_waiting_commits=long_waiting_commits,
					project_members=project_members,
					project_backlog_burndown=project_backlog_burndown,
					bigboat_status=bigboat_status)
	definitions <- yaml.load_file('analysis_definitions.yml')
	analysis_definitions <- modifyList(lapply(definitions$fields,
											  function(define) { define$field }),
									   analysis_variables) 
	items <- load_queries('analysis_reports.yml', 'sprint_definitions.yml',
						  analysis_definitions)
	lapply(items, function(item) {
		item$report <- reports[[item$table]]
		return(item)
	})
}
