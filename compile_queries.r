# Script to convert MonetDB SQL queries with patterns to executable queries.

source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/log.r')
source('include/analysis_reports.r')

make_opt_parser(desc="Compile SQL queries with patterns to executable queries",
                options=list(
                             make_option('--project-ids', default='0',
                                         help='Anonymize projects (0 or 1)'),
                             make_option('--days', default=NA_integer_,
                                         help=paste('Number of days before a',
                                                    'sprint is left out')),
                             make_option('--patch', action='store_true',
                                         default=NA,
                                         help=paste('Exclude patch sprints',
                                                    '(inverse: include only,',
                                                    'default: no filter)')),
                             make_option('--latest-date',
                                         default=as.character(Sys.time()),
                                         help=paste('Sprint start date/time',
                                                    'after which later sprints',
                                                    'are left out')),
                             make_option('--core', action='store_true',
                                         default=FALSE,
                                         help=paste('Only consider non-support',
                                                    'team, main projects')),
                             make_option('--connect', action='store_true',
                                         default=TRUE,
                                         help='Connect to DB for sprint IDs')),
                variables=analysis_definitions$fields)
config <- get_config()
arguments <- config$args
log_setup(arguments)

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

project_ids <- arguments$project_ids
if (project_ids != '0') {
    project_ids <- '1'
}
latest_date <- as.POSIXct(arguments$latest_date)

sprint_conditions <- paste(get_sprint_conditions(latest_date='',
                                                 core=arguments$core,
                                                 sprint_days=arguments$days,
                                                 sprint_patch=arguments$patch),
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
                                       list(sprint_days=arguments$days,
                                            join_cols=join_cols))
sprint_query <- load_query(list(query=query), sprint_definitions)
if (arguments$connect) {
    conn <- connect()
    sprint_data <- dbGetQuery(conn, sprint_query$query)
    sprint_ids <- paste(sprint_data[[join_cols[2]]], collapse=',')
} else {
    sprint_ids <- '0'
}

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
