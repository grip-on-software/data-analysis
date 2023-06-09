# Utilities for retrieving lists of projects.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
# Copyright 2017-2023 Leon Helwerda
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

if (!exists('INC_PROJECT_R')) {
    source('include/database.r')
    source('include/sources.r')

    INC_PROJECT_R <- T

    get_recent_date <- function(recent) {
        if (isTRUE(recent)) {
            # Within the last three months
            date <- Sys.Date() - as.difftime(12, units="weeks")
        } else {
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
        if (is.null(join_cols)) {
            join_cols <- c('project_id', 'sprint_id')
        }
        if (is.null(patterns)) {
            patterns <- load_definitions('sprint_definitions.yml',
                                         list(join_cols=join_cols))
        }

        variables <- c(list(join_cols=join_cols), patterns)
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

        if (is.null(by)) {
            by <- join_cols[1]
        }
        if (length(joins) > 0) {
            project_fields <- names(fields)
            fields <- as.list(sprintf('${t("project")}.%s', fields))
            names(fields) <- project_fields

            by <- sprintf('${t("project")}.%s', by)
        }

        group_by <- ''
        if (must_group) {
            if (!is.null(fields$quality_display_name)) {
                fields$quality_display_name <- "${f('project_display_name')}"
            }
            group_by <- paste('GROUP BY', paste(c(fields, groups),
                                                collapse=', '))
        }

        if (!is.null(fields$quality_display_name)) {
            fields$quality_display_name <- "${s(project_display_name)}"
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

    get_meta_keys <- function(project_metadata, date) {
        meta_keys <- strsplit(project_metadata, ',')[[1]]
        metadata <- vector("list", length(meta_keys))
        names(metadata) <- meta_keys
        metadata[] <- T
        if (!is.null(metadata$recent) && !missing(date)) {
            metadata$recent <- date
        }
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

    get_project_fixversions <- function(conn, project_col) {
        query <- paste('SELECT project.', project_col, ',
                        fixversion.id AS fixversion, fixversion.name
                        FROM gros.fixversion
                        JOIN gros.project
                        ON fixversion.project_id = project.project_id
                        ORDER BY fixversion.project_id, fixversion.start_date',
                        sep='')
        versions <- dbGetQuery(conn, query)
        projects <- versions[[project_col]]
        versions[[project_col]] <- NULL
        return(lapply(split(versions, projects), function(project_versions) {
            return(setNames(as.list(project_versions$name),
                            project_versions$fixversion))
        }))
    }

    write_projects_sources <- function(conn, projects, sources=NA_character_,
                                       project_ids='0', join_cols=NULL,
                                       output_directory='output') {
        if (is.null(join_cols)) {
            join_cols <- c('project_id')
        }
        ids <- projects[[join_cols[1]]]
        urls <- get_source_urls(conn, ids, sources=sources,
                                one=length(sources) == 1 && sources != 'all')
        if (length(urls) > 0) {
            if (project_ids != '0') {
                names(urls) <- paste('Proj', names(urls), sep='')
            } else {
                names(urls) <- projects[ids %in% names(urls), 'name']
            }
        }

        write(toJSON(urls, auto_unbox=T),
              file=paste(output_directory, "projects_sources.json", sep="/"))
    }

    write_projects_metadata <- function(conn, fields, metadata, projects=NA,
                                        project_ids='0', sprint_ids='0',
                                        project_sources=c(),
                                        output_directory='output',
                                        patterns=list(), join_cols=NULL,
                                        fixversions=F) {
        if (is.atomic(projects) && is.na(projects)) {
            projects <- get_projects_meta(conn, fields=fields,
                                          metadata=metadata, patterns=patterns,
                                          join_cols=join_cols)
        } else {
            if (!is.null(names(fields))) {
                fields <- names(fields)
            }
            projects <- projects[, c(as.character(fields), names(metadata))]
            if (project_ids != '0' && "component" %in% names(metadata)) {
                projects[!(projects$component %in% c(T, F)), 'component'] <- T
            }
        }

        if (length(project_sources) > 0) {
            write_projects_sources(conn, projects, sources=project_sources,
                                   project_ids=project_ids, join_cols=join_cols,
                                   output_directory=output_directory)
        }

        if (is.null(join_cols)) {
            project_col <- 'project_id'
        } else {
            project_col <- join_cols[1]
        }
        join_col <- ifelse(project_ids != '0', project_col, 'name')

        if (sprint_ids != '1' && fixversions) {
            projects$fixversions <- list(setNames(list(), character(0)))
            versions <- get_project_fixversions(conn, join_col)
            with_versions <- projects[[join_col]] %in% names(versions)
            names <- projects[with_versions, join_col]
            projects[with_versions, 'fixversions'] <- list(versions[names])
        }

        if (project_ids != '0' && nrow(projects) > 0) {
            projects$name <- paste('Proj', projects[[project_col]], sep='')
            projects$quality_display_name <- NULL
        }
        projects <- projects[order(projects[[join_col]]), ]
        projects[[project_col]] <- NULL
        write(toJSON(projects, auto_unbox=T),
              file=paste(output_directory, 'projects_meta.json', sep='/'))
    }
}
