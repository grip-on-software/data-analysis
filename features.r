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
project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
	project_ids = '1'
}
features <- get_arg('--features', default=NA)
exclude <- get_arg('--exclude', default='^$')
core <- get_arg('--core', default=F)
recent <- get_arg('--recent', default=F)

config <- get_config()
patterns <- load_definitions('sprint_definitions.yml', config$fields)

project_metadata <- get_arg('--project-metadata', default='recent,core,main')
metadata <- get_meta_keys(project_metadata)
fields <- c('project_id', 'name', 'quality_display_name')

if (get_arg('--project', default=F)) {
	result <- get_project_features(conn, features, exclude, NULL, core=core)
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

	if (project_ids != '1') {
		links <- mapply(build_source_urls,
						result$data[['project_id']],
						result$data[['name']],
						MoreArgs=list(patterns=patterns, items=result$items,
									  conn=conn),
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

	write_projects_metadata(conn, fields, metadata, projects=NA,
							project_ids=project_ids,
							output_directory=output_directory)
} else if (recent) {
	if (isTRUE(recent)) {
		recent <- 5
	}
	split <- get_arg('--split', default=F)
	old <- get_arg('--old', default=F)
	closed <- get_arg('--closed', default=F)
	specifications <- yaml.load_file('sprint_features.yml')
	all_features <- unlist(sapply(specifications$files, function(item) { item$column }))
	if (is.na(features)) {
		features <- all_features 
	}
	else {
		features <- strsplit(features, ",")[[1]]
	}

	if (split) {
		sprint_meta <- c('sprint_name', 'sprint_num', 'sprint_id', 'board_id',
						 'start_date', 'close_date')
	}
	else {
		sprint_meta <- c('sprint_name', 'sprint_num', 'start_date', '')
	}

	if (!closed) {
		sprint_meta <- c(sprint_meta, 'sprint_is_closed', 'sprint_is_complete')
	}
	meta_features <- sprint_meta[sprint_meta %in% all_features]
	default <- c(sprint_meta, 'num_story_points', 'done_story_points',
				  'velocity', 'lines_of_code', 'unittest_line_coverage')
	default_features <- default[default %in% c(sprint_meta, features)]

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
	result <- get_recent_sprint_features(conn,
										 unique(c(meta_features, features)),
										 limit=recent,
										 closed=closed,
										 sprint_meta=sprint_meta,
										 sprint_conditions=sprint_conditions,
										 project_fields=fields,
										 project_meta=metadata,
										 old=old)
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

		old_sprint_data <- sprint_data[sprint_data$old,]
		sprint_data <- sprint_data[!sprint_data$old,]

		for (project in projects) {
			project_dir = paste(output_dir, project, sep="/")
			if (!dir.exists(project_dir)) {
				dir.create(project_dir)
			}
			project_data <- sprint_data[sprint_data$project_name == project,]
			old_project_data <- old_sprint_data[old_sprint_data$project_name == project,]
			write(toJSON(project_data[,default_features]),
				  file=paste(project_dir, 'default.json', sep='/'))
			write(toJSON(old_project_data[,unique(c(sprint_meta,features))], auto_unbox=T),
				  file=paste(project_dir, 'old.json', sep='/'))

			for (feature in features) {
				if (!(feature %in% default_features)) {
					write(toJSON(project_data[[feature]], auto_unbox=T,
								 na="null", null="null"),
						  file=paste(project_dir,
							  		 paste(feature, 'json', sep='.'),
									 sep='/'))
				}
			}

			project_id <- project_data$project_id[[1]]
			sprint <- c(project_data[nrow(project_data),sprint_meta],
						list(quality_name=project_data$quality_name[[1]]))

			project_details <- lapply(result$details, function(details) {
				project <- Filter(function(detail) {
					return(detail$project_id == project_id && detail$sprint_id %in% sprint_data$sprint_id)
				}, details)
				feature_details <- Map(function(detail) {
					detail$project_id <- NULL
					detail$sprint_id <- NULL
					return(unbox(detail))
				}, project)
				names(feature_details) <- Map(function(detail) {
					return(detail$sprint_id)
				}, project)
				return(feature_details)
			})
			write(toJSON(project_details),
				  file=paste(project_dir, "details.json", sep="/"))
			write(toJSON(build_sprint_source_urls(conn, project_id, project,
												  sprint, # latest sprint
												  specifications, patterns)),
		  		  file=paste(project_dir, "links.json", sep="/"))
		}

		write_feature_metadata(projects, specifications, output_dir)
		write(toJSON(list(limit=recent, closed=closed, old=old), auto_unbox=T),
			  file=paste(output_dir, "sprints.json", sep="/"))
		write(toJSON(list(default=default_features,
						  all=features,
						  meta=sprint_meta)),
			  file=paste(output_dir, "features.json", sep="/"))
		write_projects_metadata(conn, fields, metadata,
								projects=result$projects,
								project_ids=project_ids,
								output_directory=output_dir)
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
	metrics <- get_arg('--metrics', default=F)
	combine <- get_arg('--combine', default=F)

	result <- get_sprint_features(conn, features, exclude, NULL, latest_date,
								  core=core, metrics=metrics,
								  sprint_days=days, sprint_patch=patch,
								  combine=combine)
	sprint_data <- result$data

	write.arff(sprint_data[,result$colnames],
			   file=paste(output_directory, "sprint_features.arff", sep="/"),
			   relation="sprint_data")
}
