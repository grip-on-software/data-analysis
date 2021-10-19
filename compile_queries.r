source('include/database.r')
source('include/features.r')
source('include/log.r')

export <- function(items, prefix, fields) {
    seen <- c()
    for (item in items) {
        for (field in fields) {
            name <- item[[field]]
            if (is.null(name) || length(name) > 1 || (name %in% seen)) {
                next
            }

            seen <- c(seen, name)
            write(paste(item$query, ';', sep=''),
                  file=paste('output', paste(paste(prefix, name, sep='_'),
                             'sql', sep='.'), sep='/'))
        }
    }
}

project_ids <- get_arg('--project-ids', default='0')
latest_date <- as.POSIXct(get_arg('--latest-date', default=Sys.time()))

core <- get_arg('--core', default=F)
sprint_days <- get_arg('--days', default=NA)
sprint_patch <- ifelse(get_arg('--patch', default=F), NA, F)
sprint_conditions <- paste(get_sprint_conditions(latest_date='', core=core,
                                                 sprint_days=sprint_days,
                                                 sprint_patch=sprint_patch),
                           collapse=' AND ')

config <- get_config()
if (config$db$primary_source == "tfs") {
    join_cols <- c('team_id', 'sprint_id')
} else {
    join_cols <- c('project_id', 'sprint_id')
}

query <- paste('SELECT ${f(join_cols, "sprint")}',
               'FROM gros.${t("sprint")}',
               'JOIN gros.${t("project")}',
               'ON ${j(join_cols, "project", "sprint", mask=1)}',
               'WHERE', sprint_conditions)
sprint_definitions <- load_definitions('sprint_definitions.yml',
                                       list(sprint_days=sprint_days,
                                            join_cols=join_cols))
sprint_query <- load_query(list(query=query), sprint_definitions)
conn <- connect()
sprint_data <- dbGetQuery(conn, sprint_query$query)
sprint_ids <- paste(sprint_data[[join_cols[2]]], collapse=',')

definitions <- yaml.load_file('analysis_definitions.yml')
analysis_definitions <- c(lapply(definitions$fields,
                                 function(define) { define$field }),
                          list(project_ids=project_ids,
                               join_cols=join_cols))

export(load_queries('sprint_features.yml', 'sprint_definitions.yml',
                    list(join_cols=join_cols,
                         sprint_conditions=sprint_conditions,
                         sprint_ids=sprint_ids),
                    current_time=latest_date),
       'feature', c('column', 'table'))
export(load_queries('sprint_events.yml', 'sprint_definitions.yml',
                    list(project_ids=project_ids,
                         join_cols=join_cols),
                    current_time=latest_date),
       'event', 'type')
export(load_queries('analysis_reports.yml', 'sprint_definitions.yml',
                    analysis_definitions,
                    current_time=latest_date),
       'report', 'table')
export(load_queries('project_features.yml', 'sprint_definitions.yml'),
       'project', 'column')
