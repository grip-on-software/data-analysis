# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
conn <- connect()

get_source_urls <- function(conn, project_id) {
	vcs_sources <- c('svn', 'git', 'github', 'gitlab', 'tfs')
	environments <- dbGetQuery(conn, paste('SELECT source_type, url FROM gros.source_environment WHERE project_id = ', project_id, sep=''))
	urls <- as.list(sub("/$", "", environments$url))
	if (length(urls) == 0) {
		return(urls)
	}
	names(urls) <- paste(environments$source_type, 'url', sep='_')

	for (vcs_source in vcs_sources) {
		if (vcs_source %in% environments$source_type) {
			urls$vcs_url <- urls[[paste(vcs_source, 'url', sep='_')]]
		}
	}

	return(urls)
}

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
		data[[main_project]] <- unbox(data[[main_project]] + data[[subproject]])
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
		links <- mapply(function(project_id, project) {
			project_links <- list()
			project_urls <- get_source_urls(conn, project_id)
			project_patterns <- c(list(jira_key=project),
								  project_urls, patterns)

			for (item in result$items) {
				source_url <- str_interp(item$source, project_patterns)
				project_links[[item$column]] <- list(source=unbox(source_url))
			}
			return(project_links)
		}, result$data[['project_id']], result$data[['name']], SIMPLIFY=F)
		names(links) <- result$data[['name']]
	}
	else {
		links <- list()
	}
	write(toJSON(links),
		  file=paste(output_directory, "project_features_links.json", sep="/"))
	loginfo("Wrote project_features_links.json")

	locale <- list()
	for (item in result$items) {
		locale[[item$column]] <- unbox(item$name)
	}
	write(toJSON(locale),
		  file=paste(output_directory, "project_features_localization.json",
		  			 sep="/"))
	loginfo("Wrote project_features_localization.json")

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
	result <- get_sprint_features(conn, exclude, NULL, latest_date, core=core)
	sprint_data <- result$data

	write.arff(sprint_data[,result$colnames],
			   file=paste(output_directory, "sprint_features.arff", sep="/"),
			   relation="sprint_data")
}

