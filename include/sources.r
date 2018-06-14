# Functions for obtaining source information

source('include/database.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%d %H:%M:%S")
}

get_source_urls <- function(conn, project_id, sources='all') {
	vcs_sources <- c('svn', 'git', 'github', 'gitlab', 'tfs')
	conditions = list(project_id=paste('project_id =', project_id))
	if (sources != 'all') {
		if ('vcs' %in% sources) {
			sources <- c(sources, vcs_sources)
		}
		conditions$source_type = paste('source_type IN (',
									   paste(dbQuoteString(conn, sources),
									   		 collapse=','), ')')
	}
	environments <- dbGetQuery(conn,
							   paste('SELECT source_type, url
							   		 FROM gros.source_environment
									 WHERE',
									 paste(conditions, collapse=' AND ')))
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

get_source_pattern <- function(item, project_urls) {
	if (is.list(item$source)) {
		url_names <- paste(names(item$source), 'url', sep='_')
		index = which(url_names %in% names(project_urls))
		if (length(index) > 0) {
			return(item$source[[index[1]]])
		}
	}
	else if (!is.null(item$source)) {
		return(item$source)
	}
	return(NA)
}

build_source_urls <- function(project_id, project_name, items=list(), patterns=c(), conn=NA) {
	project_links <- list()
	if (is.list(conn)) {
		project_urls <- conn
	}
	else {
		project_urls <- get_source_urls(conn, project_id)
	}
	project_patterns <- c(list(jira_key=project_name),
						  project_urls, patterns)

	for (item in items) {
		item_source <- get_source_pattern(item, project_urls)
		if (!is.na(item_source)) {
			source_url <- str_interp(item_source, project_patterns)
			project_links[[item$column]] <- list(source=unbox(source_url))
			if ("type" %in% names(item)) {
				project_links[[item$column]]$type <- unbox(item$type)
			}
			else if ("groups" %in% names(item)) {
				project_links[[item$column]]$type <- unbox(item$groups[[1]])
			}
		}
	}
	return(project_links)
}

build_sprint_source_urls <- function(conn, project_id, project_name, sprint,
									 specifications, patterns) {
	source_urls <- get_source_urls(conn, project_id)
	source_items <- list()
	for (item in specifications$files) {
		if (is.list(item$source)) {
			url_names <- paste(names(item$source), 'url', sep='_')
			index = which(url_names %in% names(source_urls))
			if (length(index) > 0) {
				source_items[[item$column]] <- c(item, list(source=item$source[[index[1]]],
															type=names(item$source)[[index[1]]]))
			}
		}
		else if (!is.null(item$source)) {
			source_items[[item$column]] <- item
		}
	}

	sprint_patterns <- list(jira_board_id=sprint$board_id,
							jira_sprint_id=sprint$sprint_id,
							quality_name=ifelse(is.na(sprint$quality_name), '',
												sprint$quality_name),
							sprint_start_date=dateFormat(sprint$start_date),
							sprint_end_date=dateFormat(sprint$close_date))
	return(build_source_urls(project_id, project_name, items=source_items,
							 patterns=c(patterns, sprint_patterns),
							 conn=source_urls))
}
