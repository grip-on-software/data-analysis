# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/sources.r')
conn <- connect()

output_directory <- get_arg('--output', default='output')
config_file <- get_arg('--config', default='config.yml')
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids = '1'
}
exclude <- get_arg('--exclude', default='^$')
core <- get_arg('--core', default=F)
recent <- get_arg('--recent', default=F)

if (get_arg('--project', default=F)) {
	result <- get_project_features(conn, exclude, NULL, core=core)
	subprojects <- get_subprojects(conn)

	df <- result$data[,result$colnames]
	data <- lapply(as.list(split(df, seq(nrow(df)))), unbox)
	names(data) <- result$data[['name']]
	for (subproject in subprojects$name) {
		main_project <- subprojects[subprojects$name == subproject,'main_project']
		if (main_project %in% data) {
			data[[main_project]] <- unbox(data[[main_project]] + data[[subproject]])
		}
		data[subproject] <- NULL
	}
	if (project_ids == '1') {
		names(data) <- paste("Proj",
							 result$data[['project_id']][result$data[['name']] %in% names(data)],
							 sep="")
	}
	write(toJSON(data),
		  file=paste(output_directory, "project_features.json", sep="/"))
	loginfo("Wrote project_features.json")

	normalize <- lapply(result$items, function(item) { unbox(item$normalize) })
	names(normalize) <- result$colnames
	write(toJSON(normalize, null="null"),
		  file=paste(output_directory, "project_features_normalize.json",
		  			 sep="/"))
	loginfo("Wrote project_features_normalize.json")

	config <- yaml.load_file(config_file)
	patterns <- load_definitions('sprint_definitions.yml', config$fields)
	if (project_ids != '1') {
		links <- mapply(build_source_urls,
						result$data[['project_id']],
						result$data[['name']],
						MoreArgs=list(patterns=patterns, items=result$items),
						SIMPLIFY=F)
		names(links) <- result$data[['name']]
	}
	else {
		links <- list()
	}
	write(toJSON(links),
		  file=paste(output_directory, "project_features_links.json", sep="/"))
	loginfo("Wrote project_features_links.json")

	write(toJSON(get_feature_locales(result$items)),
		  file=paste(output_directory, "project_features_locales.json",
		  			 sep="/"))
	loginfo("Wrote project_features_locales.json")

	write(toJSON(get_locales(yaml.load_file("source_types.yml"))),
		  file=paste(output_directory, "project_features_sources.json",
		  			 sep="/"))
	loginfo("Wrote project_features_sources.json")

	groups <- list()
	for (item in result$items) {
		groups[[item$column]] <- item$groups
	}
	write(toJSON(groups),
		  file=paste(output_directory, "project_features_groups.json", sep="/"))
	loginfo("Wrote project_features_groups.json")
} else if (recent) {
	if (isTRUE(recent)) {
		recent <- 5
	}
	split <- get_arg('--split', default=F)
	closed <- get_arg('--closed', default=F)
	specifications <- yaml.load_file('sprint_features.yml')
	features <- c('sprint_num', 'num_story_points', 'num_stories',
				  'num_not_done', 'num_removed_stories', 'num_added_stories',
				  'num_done_stories', 'done_story_points', 'velocity')

	if (split) {
		sprint_meta <- c('sprint_name', 'sprint_num', 'sprint_id', 'board_id',
						 'start_date', 'close_date')
	}
	else {
		sprint_meta <- c('sprint_name', 'sprint_num', 'start_date')
	}
	default_features <- c(sprint_meta, 'num_story_points', 'done_story_points',
						  'velocity')

	if (closed) {
		features <- c(features, 'sprint_is_closed')
		default_features <- c(default_features, 'sprint_is_closed')
	}

	core <- get_arg('--core', default=F)
	sprint_days <- get_arg('--days', default=NA)
	sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
	conditions <- get_sprint_conditions(latest_date='', core=core,
										sprint_days=sprint_days,
										sprint_patch=sprint_patch)
	if (length(conditions) != 0) {
		sprint_conditions <- paste('AND', paste(conditions, collapse=' AND '))
	}
	else {
		sprint_conditions <- ''
	}
	result <- get_recent_sprint_features(conn, features,
										 limit=recent,
										 closed=closed,
										 sprint_meta=sprint_meta,
										 sprint_conditions=sprint_conditions)
	sprint_data <- result$data
	if (project_ids != '0') {
		sprint_data$project_name <- paste("Proj", sprint_data$project_id, sep="")
	}
	if (split) {
		output_dir <- paste(output_directory, 'recent_sprint_features', sep="/")
		if (!dir.exists(output_dir)) {
			dir.create(output_dir)
		}
		projects <- levels(factor(sprint_data$project_name))
		for (project in projects) {
			project_dir = paste(output_dir, project, sep="/")
			if (!dir.exists(project_dir)) {
				dir.create(project_dir)
			}
			write(toJSON(sprint_data[sprint_data$project_name == project,
									 default_features], auto_unbox=T),
				  file=paste(project_dir, 'default.json', sep='/'))

			for (feature in result$colnames) {
				if (!(feature %in% default_features)) {
					write(toJSON(sprint_data[sprint_data$project_name == project,feature],
								 auto_unbox=T),
						  file=paste(project_dir,
							  		 paste(feature, 'json', sep='.'),
									 sep='/'))
				}
			}
		}
		write_feature_metadata(projects, specifications, output_dir)
		quality_names <- lapply(projects, function(project) {
			sprint_data[sprint_data$project_name == project,'quality_display_name'][[1]]
		})
		names(quality_names) <- projects
		write(toJSON(quality_names, auto_unbox=T),
			  file=paste(output_dir, "quality_names.json", sep="/"))
		write(toJSON(list(default=default_features,
						  all=features,
						  meta=sprint_meta)),
			  file=paste(output_dir, "features.json", sep="/"))
	}
	else {
		write.csv(sprint_data[,result$colnames],
				  file=paste(output_directory, 'recent_sprint_features.csv',
				  			 sep="/"),
				  row.names=F)
	}
} else {
	latest_date <- get_arg('--latest-date', default='')
	days <- get_arg('--days', default=NA)
	patch <- ifelse(get_arg('--patch', default=F), NA, F)

	result <- get_sprint_features(conn, exclude, NULL, latest_date, core=core,
								  metrics=get_arg('--metrics', default=F),
								  sprint_days=days, sprint_patch=patch)
	sprint_data <- result$data

	write.arff(sprint_data[,result$colnames],
			   file=paste(output_directory, "sprint_features.arff", sep="/"),
			   relation="sprint_data")
}

