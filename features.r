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

	write(toJSON(yaml.load_file("source_types.yml"), auto_unbox=T),
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
} else if (get_arg('--recent', default=F)) {
	features <- c('num_story_points', 'num_stories', 'num_not_done',
				  'num_removed_stories', 'num_added_stories',
				  'num_done_stories')
	result <- get_recent_sprint_features(conn, features)
	sprint_data <- result$data
	write.csv(sprint_data[,result$colnames],
			  file=paste(output_directory, 'recent_sprint_features.csv',
			  			 sep="/"),
			  row.names=F)
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

