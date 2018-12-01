# Functions for obtaining source information

source('include/database.r')

dateFormat <- function(date) {
    format(as.POSIXct(date), format="%Y-%m-%d %H:%M:%S")
}

url_names <- function(sources) {
    paste(sources, 'url', sep='_')
}

vcs_sources <- c('svn', 'git', 'github', 'gitlab', 'tfs')
filter_project_urls <- function(project, one, default_urls) {
    if (one) {
        project <- project[!duplicated(project$source_type), ]
    }
    project_urls <- as.list(sub("(/)$", "", project$url))
    if (length(project_urls) == 0) {
        names(project_urls) <- list()
    }
    else {
        names(project_urls) <- url_names(project$source_type)
    }
    urls <- modifyList(default_urls[names(default_urls) != 'vcs_url'],
                       project_urls)

    for (vcs_source in vcs_sources) {
        if (vcs_source %in% project$source_type) {
            url <- urls[[url_names(vcs_source)]]
            if (!("vcs_url" %in% names(urls)) ||
                startsWith(urls$vcs_url, url)) {
                urls$vcs_url <- url
            }
        }
    }
    if (one && "vcs_url" %in% names(urls)) {
        urls <- list(vcs_url=urls$vcs_url)
    }

    return(urls)
}

get_source_urls <- function(conn, project_id, sources='all', web=T, one=F) {
    conditions <- list(project_id=paste('project_id IN (',
                                        paste(project_id, collapse=','), ')'))
    config <- get_config()
    default_urls <- config$fields[endsWith(names(config$fields), '_url')]
    if (sources != 'all') {
        if ('vcs' %in% sources) {
            sources <- c(sources, vcs_sources)
        }
        conditions$source_type <- paste('source_type IN (',
                                        paste(dbQuoteString(conn, sources),
                                              collapse=','), ')')
        default_sources <- names(default_urls) %in% url_names(sources)
        default_urls <- default_urls[default_sources]
    }
    if (web) {
        conditions$web <- "(url LIKE 'http://%' OR url LIKE 'https://%')"
        conditions$specified <- "url <> 'http://unspecified'"
    }

    order <- 'project_id, source_type, url'
    if (one && length(project_id) == 1 && 'vcs' %in% sources) {
        order <- paste(order, 'LIMIT 1')
    }
    environments <- dbGetQuery(conn,
                               paste('SELECT project_id, source_type, url
                                        FROM gros.source_environment
                                     WHERE',
                                     paste(conditions, collapse=' AND '),
                                     'ORDER BY', order))
    projects <- lapply(split(environments, environments$project_id),
                       filter_project_urls, one, default_urls)

    if (length(project_id) == 1 && length(projects) == 1) {
        return(projects[[as.character(project_id)]])
    }
    return(projects)
}

get_source_pattern <- function(item, project_urls) {
    if (is.list(item$source)) {
        index <- which(url_names(names(item$source)) %in% names(project_urls))
        if (length(index) > 0 && !is.null(item$source[[index[1]]])) {
            return(item$source[[index[1]]])
        }
    }
    else if (!is.null(item$source)) {
        return(item$source)
    }
    return(NA)
}

build_source_urls <- function(project_id, project_name, items=list(),
                              patterns=c(), conn=NA, team_projects=c()) {
    project_links <- list()
    names(project_links) <- list()
    if (is.list(conn)) {
        project_urls <- conn
    }
    else {
        project_urls <- get_source_urls(conn, project_id)
    }
    project_patterns <- c(list(jira_key=ifelse(length(team_projects) > 0,
                                               team_projects[[1]],
                                               project_name),
                               jira_keys=paste(team_projects, collapse=',')),
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

build_project_source_urls <- function(conn, project_id, project_name, patterns,
                                      sources='all', team_projects=c()) {
    metric_options_url <- paste("${metric_options_url}", "blob/master",
                                "${metric_options_file}", sep="/")
    quality_url <- "${quality_url}/${quality_name}"
    if (length(team_projects) > 0 && "board_id" %in% names(patterns)) {
        jira_url <- paste("${jira_url}/secure/RapidBoard.jspa",
                          "?rapidView=${board_id}&view=planning", sep="")
    }
    else {
        jira_url <- "${jira_url}/browse/${project_name}"
    }
    config <- get_config()
    default_urls <- config$fields[endsWith(names(config$fields), '_url')]
    source_types <- names(yaml.load_file('source_types.yml'))
    types <- unique(c(names(default_urls), url_names(source_types)))
    plain_urls <- as.list(paste("${", types, "}", sep=""))
    names(plain_urls) <- types
    urls <- modifyList(plain_urls, list(project_url=metric_options_url,
                                        quality_url=quality_url,
                                        metric_history_url=quality_url,
                                        metric_options_url=metric_options_url,
                                        jira_url=jira_url))
    if (is.list(conn)) {
        source_urls <- conn
    }
    else {
        source_urls <- get_source_urls(conn, project_id, sources=sources)
    }

    project_links <- list()
    for (name in names(source_urls)) {
        if (name %in% names(urls)) {
            type <- gsub('_url$', '', name)
            patterns <- c(list(project_name=project_name),
                          source_urls, patterns, config$fields)
            project_links[[type]] <- str_interp(urls[[name]], patterns)
        }
    }
    return(project_links)
}

build_sprint_source_urls <- function(conn, project_id, project_name,
                                     quality_name, sprint,
                                     items, patterns, team_projects=c()) {
    if (is.list(conn)) {
        source_urls <- conn
    }
    else {
        source_urls <- get_source_urls(conn, project_id)
    }
    source_items <- list()
    for (item in items) {
        if (is.list(item$source)) {
            urls <- url_names(names(item$source))
            index <- which(urls %in% names(source_urls))
            if (length(index) > 0) {
                new_item <- list(source=item$source[[index[1]]],
                                 type=names(item$source)[[index[1]]])
                source_items[[item$column]] <- c(item, new_item)
            }
        }
        else if (!is.null(item$source)) {
            source_items[[item$column]] <- item
        }
    }

    if (is.null(sprint)) {
        sprint_patterns <- list(jira_board_id='{{board_id}}',
                                jira_sprint_id='{{sprint_id}}',
                                jira_sprint_ids='{{sprint_ids}}',
                                sprint_start_date='{{start_date}}',
                                sprint_end_date='{{end_date}}')
    }
    else {
        sprint_patterns <- list(jira_board_id=sprint$board_id[1],
                                jira_sprint_id=sprint$sprint_id[1],
                                jira_sprint_ids=paste(sprint$sprint_id,
                                                      collapse=","),
                                sprint_start_date=dateFormat(sprint$start_date),
                                sprint_end_date=dateFormat(sprint$close_date))
    }
    sprint_patterns$quality_name <- ifelse(is.na(quality_name), '',
                                           quality_name)
    return(build_source_urls(project_id, project_name, items=source_items,
                             patterns=c(patterns, sprint_patterns),
                             conn=source_urls, team_projects=team_projects))
}
