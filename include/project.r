# Utilities for retrieving lists of projects.

if (!exists('INC_PROJECT_R')) {
    source('include/database.r')
    source('include/sources.r')

    INC_PROJECT_R <- T

    get_recent_date <- function(recent) {
        if (isTRUE(recent)) {
            # Within the last three months
            date <- Sys.Date() - as.difftime(12, units="weeks")
        }
        else {
            date <- as.Date(recent)
        }
        return(date)
    }

    get_projects_meta <- function(conn, fields=list('project_id', 'name'),
                                  metadata=list(),
                                  join_cols=c('project_id', 'sprint_id'),
                                  patterns=NULL, by=NULL) {
        joins <- list()
        aliases <- list()
        groups <- c()
        must_group <- F
        if (is.null(names(fields))) {
            names(fields) <- fields
        }
        if (is.null(patterns)) {
            patterns <- load_definitions('sprint_definitions.yml',
                                         list(join_cols=join_cols))
        }

        variables <- c(patterns, list(join_cols=join_cols))
        if (!is.null(metadata$recent)) {
            date <- get_recent_date(metadata$recent)
            variables$date <- date
            aliases$recent <- '${s(project_recent)}'
            joins$sprint <- '${j(join_cols, "project", "sprint", mask=1)}'
            must_group <- T
        }
        if (!is.null(metadata$core)) {
            aliases$core <- '${s(project_core)}'
            groups <- c(groups, '${f("project_core")}')
        }
        if (!is.null(metadata$main)) {
            aliases$main <- '${s(project_main)}'
            groups <- c(groups, '${f("project_main")}')
        }

        if (is.null(join_cols)) {
            join_cols <- c('project_id', 'sprint_id')
        }
        if (is.null(by)) {
            by <- join_cols[1]
        }
        if (length(joins) > 0) {
            project_fields <- names(fields)
            fields <- sprintf('${t("project")}.%s', fields)
            names(fields) <- project_fields

            by <- sprintf('${t("project")}.%s', by)
        }
        group_by <- ''
        if (must_group) {
            group_by <- paste('GROUP BY', paste(c(fields, groups),
                                                collapse=', '))
        }
        fields <- c(fields, aliases)

        query <- paste('SELECT', paste(format_aliases(fields), collapse=', '),
                       'FROM gros.${t("project")}',
                       paste(mapply(function(table, cond) {
                                        paste('JOIN gros.${t("', table, '")} ',
                                              'ON ', cond, sep="")
                             },
                             names(joins), joins), collapse=' '),
                       group_by,
                       'ORDER BY', paste(by, collapse=', '))
        item <- load_query(list(query=query), variables)
        logdebug(item$query)
        dbGetQuery(conn, item$query)
    }

    get_projects <- function(conn, by='project_id') {
        dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
                               ORDER BY', by))
    }

    get_main_projects <- function(conn, by='project_id') {
        dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
                               WHERE main_project IS NULL ORDER BY', by))
    }

    get_subprojects <- function(conn, by='project_id') {
        dbGetQuery(conn, paste('SELECT project_id, name, main_project
                               FROM gros.project WHERE main_project IS NOT NULL
                               ORDER BY', by))
    }

    get_core_projects <- function(conn, by='project_id') {
        dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
                               WHERE COALESCE(is_support_team, false) = false
                               ORDER BY', by))
    }

    get_sprint_projects <- function(conn, by=NULL, patterns=NULL,
                                    join_cols=c('project_id', 'sprint_id')) {
        if (is.null(by)) {
            by <- join_cols[1]
        }
        if (is.null(patterns)) {
            patterns <- load_definitions('sprint_definitions.yml',
                                         list(join_cols=join_cols))
        }

        variables <- c(patterns, list(join_cols=join_cols))
        query <- paste('SELECT ${f(join_cols, "sprint_project", mask=1)}, name
                        FROM (
                            SELECT DISTINCT ${f(join_cols, "project", mask=1)},
                            ${t("project")}.name
                            FROM gros.${t("project")}
                            JOIN gros.${t("sprint")}
                            ON ${j(join_cols, "project", "sprint", mask=1)}
                        ) AS sprint_project
                        ORDER BY', by)
        item <- load_query(list(query=query), variables)
        logdebug(item$query)
        dbGetQuery(conn, item$query)
    }

    get_repo_projects <- function(conn, by='project_id') {
        dbGetQuery(conn, paste('SELECT project_id, name FROM
                               (SELECT DISTINCT project.project_id, project.name
                               FROM gros.project
                               JOIN gros.repo
                               ON project.project_id = repo.project_id)
                               AS repo_project
                               ORDER BY', by))
    }

    get_recent_projects <- function(conn, date) {
        # Retrieve projects that have had a sprint recently
        if (missing(date)) {
            # Within the last three months
            date <- Sys.Date() - as.difftime(12, units="weeks")
        }
        query <- paste('SELECT project.project_id, project.name
                       FROM gros.sprint
                       JOIN gros.project
                       ON sprint.project_id = project.project_id
                       WHERE start_date >= CAST(\'', date, '\' AS TIMESTAMP)
                       GROUP BY project.project_id, project.name
                       ORDER BY project.project_id', sep='')
        dbGetQuery(conn, query)
    }

    get_meta_keys <- function(project_metadata) {
        meta_keys <- strsplit(project_metadata, ',')[[1]]
        metadata <- vector("list", length(meta_keys))
        names(metadata) <- meta_keys
        metadata[] <- T
        return(metadata)
    }

    get_project_intervals <- function(conn, count=4) {
        query <- 'SELECT project_id, MIN(start_date) AS first,
                  MAX(COALESCE(complete_date, end_date)) AS last
                  FROM gros.sprint GROUP BY project_id'
        range <- dbGetQuery(conn, query)
        intervals <- mapply(function(project_id, first, last) {
                                seq.Date(first, last, length.out=4)
                            },
                            as.character(range$project_id),
                            as.Date(range$first), as.Date(range$last),
                            USE.NAMES=T)

        return(intervals)
    }

    write_projects_sources <- function(conn, projects, sources=NA,
                                       output_directory='output') {
        urls <- get_source_urls(conn, projects$project_id,
                                sources=sources,
                                one=length(sources) == 1 && sources != 'all')
        if (project_ids != '0') {
            names(urls) <- paste('Proj', names(sources), sep='')
        }
        else {
            names(urls) <- projects[projects$project_id %in% names(urls),
                                    'name']
        }

        write(toJSON(urls, auto_unbox=T),
              file=paste(output_directory, "projects_sources.json", sep="/"))
    }

    write_projects_metadata <- function(conn, fields, metadata, projects=NA,
                                        project_ids='0', project_sources=c(),
                                        output_directory='output',
                                        patterns=list(), join_cols=NULL) {
        if (is.atomic(projects) && is.na(projects)) {
            projects <- get_projects_meta(conn, fields=fields,
                                          metadata=metadata, patterns=patterns,
                                          join_cols=join_cols)
        }
        else {
            if (!is.null(names(fields))) {
                fields <- names(fields)
            }
            projects <- projects[, c(as.character(fields), names(metadata))]
        }

        if (length(project_sources) > 0) {
            write_projects_sources(conn, projects, sources=project_sources,
                                   output_directory=output_directory)
        }

        if (is.null(join_cols)) {
            project_col <- 'project_id'
        }
        else {
            project_col <- join_cols[1]
        }
        if (project_ids != '0') {
            projects$name <- paste('Proj', projects[[project_col]], sep='')
            projects$quality_display_name <- NULL
            projects <- projects[order(projects[[project_col]]), ]
        }
        else {
            projects <- projects[order(projects$name), ]
        }
        projects[[project_col]] <- NULL
        write(toJSON(projects, auto_unbox=T),
              file=paste(output_directory, 'projects_meta.json', sep='/'))
    }
}
