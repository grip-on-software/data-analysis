# Functions for obtaining source information

source('include/database.r')

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

build_source_urls <- function(project_id, project_name, items=list(), patterns=c()) {
	project_links <- list()
	project_urls <- get_source_urls(conn, project_id)
	project_patterns <- c(list(jira_key=project_name),
						  project_urls, patterns)

	for (item in items) {
		source_url <- str_interp(item$source, project_patterns)
		project_links[[item$column]] <- list(source=unbox(source_url))
	}
	return(project_links)
}
