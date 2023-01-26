# Script to measure performance of several old/new queries in cold/hot starts.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
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

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/features.r')
source('include/log.r')

enable_querylog <- function(conn) {
    dbExecute(conn, 'CALL sys.querylog_empty()')
    dbExecute(conn, 'CALL sys.querylog_enable()')
    loginfo('Starting with empty, enabled query log')
}

disable_querylog <- function(conn) {
    dbExecute(conn, 'CALL sys.querylog_disable()')
    loginfo('Disabled query log')
}

cold_start <- function(conn, config, arguments) {
    dbDisconnect(conn)
    dbname <- 'gros_temp_perf'
    if (Sys.getenv('USER') == 'root') {
        # Stop/start the database while clearing memory.
        # This also clears/disables the query log.
        system(paste('monetdb stop', dbname))
        system('echo 3 > /proc/sys/vm/drop_caches')
        system(paste('monetdb start', dbname))
    }
    else if (arguments$import != '' && arguments$path != '' &&
             arguments$date != '') {
        virtual_env <- '/usr/local/envs/controller'
        loginfo('Recreating %s database by running %s %s %s in %s', dbname,
                arguments$import, arguments$org, arguments$date,
                arguments$path)
        system(paste('/bin/bash -c "cd', shQuote(arguments$path), ';',
                     paste('VIRTUAL_ENV="', virtual_env, '" PATH="',
                           virtual_env, '/bin:$PATH"', sep=''),
                     shQuote(arguments$import), 'temp_perf',
                     shQuote(arguments$date), config$db$host, '"',
                     sep=' '))
    }
    else {
        stop('Cannot use a performance test database for cold start')
    }
    config$db$dbname <<- dbname
    conn <- connect()
    enable_querylog(conn)
    loginfo('Sleeping for %d seconds', arguments$sleep)
    Sys.sleep(arguments$sleep)
    return(conn)
}

cold_start_end <- function(conn, info, intermediate, run, item) {
    # Ensure querylog results are kept between runs by merging the stats
    output <- list(as.list(collect_stats(conn, info)))

    # Dump for future reference
    write(toJSON(output[[1]]),
          file=paste('output', paste('performance', run, item$column[1],
                                     ifelse(is.null(item$old), 'new', 'old'),
                                     'json', sep='.'), sep='/'))

    if (length(intermediate) > 0) {
        output <- c(intermediate, output)
    }
    loginfo('Sleeping for %d seconds', arguments$sleep)
    Sys.sleep(arguments$sleep)
    return(output)
}

stats <- list(optimize='optimize',
              wall=paste('EXTRACT(EPOCH FROM "', c('stop', 'start'), '")',
                         sep='', collapse=' - '),
              run='run',
              ship='ship',
              load='cpu',
              io='io')
collect_stats <- function(conn, info) {
    mean_stats <- paste('AVG(', stats, ')', sep='')
    names(mean_stats) <- paste(names(stats), 'mean', sep='_')
    std_stats <- paste('sys.stddev_samp(', stats, ')', sep='')
    names(std_stats) <- paste(names(stats), 'std', sep='_')
    stats_query <- paste('SELECT query, MAX(tuples) AS columns,',
                         paste(format_aliases(mean_stats), collapse=', '), ',',
                         paste(format_aliases(std_stats), collapse=', '),
                         'FROM sys.querylog_history',
                         'GROUP BY query', sep=' ')
    logdebug(stats_query)
    data <- dbGetQuery(conn, stats_query)

    output <- list()
    normalized <- gsub('--[^\n]*\n|[\n;]+$', '',
                       gsub('\\\\', '',
                            gsub('\\\\n', '\n',
                                 tolower(data$query))))
    for (performance_query in names(info)) {
        performance_item <- info[[performance_query]]
        normalized_query <- gsub('--[^\n]*\n|[\n;]+$', '',
                                 gsub('\n\n+', '\n',
                                      gsub('[ \t]+', ' ',
                                           tolower(performance_query))))
        performance <- data[normalized == normalized_query, ]
        if (nrow(performance) == 0) {
            next
        }
        performance$rows <- performance_item$rows
        if (is.null(output[[performance_item$column[1]]])) {
            output[[performance_item$column[1]]] <- list()
        }
        if (!is.null(performance_item$old)) {
            output[[performance_item$column[1]]]$old <- performance
        }
        else {
            output[[performance_item$column[1]]]$new <- performance
        }
    }
    return(output)
}

combine_stats <- function(intermediate, info) {
    # Combine statistics from cold start runs
    output <- list()
    for (performance_query in names(info)) {
        column <- info[[performance_query]]$column[1]
        output[[column]] <- list()
        for (version in c('new', 'old')) {
            performance_inter <- Filter(function(inter) {
                                            !is.null(inter[[column]][[version]])
                                        },
                                        intermediate)
            first <- performance_inter[[1]][[column]][[version]]
            performance <- list(query=first$query,
                                columns=first$columns,
                                rows=first$rows)
            for (metric in names(stats)) {
                mean_metric <- paste(metric, 'mean', sep='_')
                std_metric <- paste(metric, 'std', sep='_')
                values <- sapply(performance_inter,
                                 function(perf) {
                                     perf[[column]][[version]][[mean_metric]]
                                 })
                performance[[mean_metric]] <- mean(values)
                performance[[std_metric]] <- sd(values)
            }
            output[[column]][[version]] <- performance
        }
    }
    return(output)
}

# Parse options
make_opt_parser(desc="Measure performance of queries in different conditions",
                options=list(make_option('--cold', action='store_true',
                                         default=FALSE,
                                         help='Enable cold-start experiment'),
                             make_option('--import', default='',
                                         help=paste('Script that imports dumps',
                                                    'into new databases.',
                                                    'Only used in --cold, and',
                                                    '--path and --date must',
                                                    'all be given.')),
                             make_option('--path', default='',
                                         help=paste('Working directory from',
                                                    'which the import script',
                                                    'is to be run. This should',
                                                    'point to a monetdb-import',
                                                    'repo Scripts directory.')),
                             make_option('--date', default='',
                                         help=paste('Date from which to import',
                                                    'the database dump. This',
                                                    'is used as a path name.',
                                                    'A file named dump.tar.gz',
                                                    'must be in this path.')),
                             make_option('--runs', default=5,
                                         help='Number of runs to perform'),
                             make_option('--sleep', default=10,
                                         help=paste('Number of seconds to wait',
                                                    'between import and query.',
                                                    'Only during cold start.')),
                             make_option('--filename',
                                         default='performance.json',
                                         help='Filename to export results to'),
                             make_option('--days', default=NA_integer_,
                                         help=paste('Number of days before a',
                                                    'sprint is left out')),
                             make_option('--patch', action='store_true',
                                         default=FALSE,
                                         help='Exclude patch sprints'),
                             make_option('--latest-date',
                                         default=as.character(Sys.time()),
                                         help=paste('Sprint start date/time',
                                                    'after which later sprints',
                                                    'are left out')),
                             make_option('--core', action='store_true',
                                         default=FALSE,
                                         help=paste('Only consider non-support',
                                                    'team, main projects'))),
                variables=get_config_fields())

config <- get_config()
arguments <- config$args
log_setup(arguments)

if (config$db$primary_source == "tfs") {
    join_cols <- c('team_id', 'sprint_id')
} else {
    join_cols <- c('project_id', 'sprint_id')
}

conn <- connect()

# Filtering options
latest_date <- as.POSIXct(arguments$latest_date)
sprint_patch <- ifelse(arguments$patch, NA, F)

sprint_conditions <- paste(get_sprint_conditions(latest_date='',
                                                 core=arguments$core,
                                                 sprint_days=arguments$days,
                                                 sprint_patch=sprint_patch),
                           collapse=' AND ')

# Retrieve sprint ID filter (NB: not part of performance/cold-start)
query <- paste('SELECT ${f(join_cols, "sprint")}',
               'FROM gros.${t("sprint")}',
               'JOIN gros.${t("project")}',
               'ON ${j(join_cols, "project", "sprint", mask=1)}',
               'WHERE', sprint_conditions)
sprint_definitions <- load_definitions('sprint_definitions.yml',
                                       c(config$fields,
                                         list(sprint_days=arguments$days,
                                              join_cols=join_cols)))
sprint_query <- load_query(list(query=query), sprint_definitions)
logdebug(sprint_query$query)
sprint_data <- dbGetQuery(conn, sprint_query$query)
filter_sprint_ids <- paste(sprint_data[[join_cols[2]]], collapse=',')

# Retrieve relevant queries and old versions
old_performance <- load_queries('old_performance.yml', static=T)
filenames <- lapply(old_performance, function(item) { item$filename })
columns <- lapply(old_performance, function(item) { item$column })
names(filenames) <- columns
queries <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
                        list(join_cols=join_cols,
                             sprint_conditions=sprint_conditions,
                             filter_sprint_ids=filter_sprint_ids),
                        current_time=latest_date, features=columns)
old_queries <- lapply(queries, function(item) {
                          item$definition <- NULL
                          item$metric <- NULL
                          item$query <- NULL
                          item$old <- 'old'
                          item$filename <- filenames[[item$column]]
                          load_query(item, item$patterns, 'old_performance')
                      })

info <- list()
intermediate <- list()
if (arguments$runs > 0) {
    # Clear out querylog, enable querylog, perform queries, disable querylog
    enable_querylog(conn)

    for (run in seq(1, arguments$runs)) {
        for (item in c(queries, old_queries)) {
            if (arguments$cold) {
                conn <- cold_start(conn, config, arguments)
            }
            loginfo('Executing %s query for table %s: column(s) %s', item$old,
                    item$table, item$column)
            logdebug(item$query)
            time <- system.time(result <- dbGetQuery(conn, item$query))
            loginfo('The %s query for table %s column(s) %s took %f seconds',
                    item$old, item$table, item$column, time['elapsed'])
            loginfo('The %s query for table %s column(s) %s has %d rows',
                    item$old, item$table, item$column, nrow(result))
            info[[item$query]] <- item
            info[[item$query]]$rows <- nrow(result)
            if (arguments$cold) {
                intermediate <- cold_start_end(conn, info, intermediate, run,
                                               item)
            }
        }
    }

    disable_querylog(conn)
} else {
    # No information on row counts
    info <- c(queries, old_queries)
    names(info) <- lapply(info, function(item) { item$query })
    if (arguments$cold) {
        for (filename in Sys.glob('output/performance.*.*.*.json')) {
            performance <- list(fromJSON(filename))
            if (length(intermediate) > 0) {
                intermediate <- c(intermediate, performance)
            }
            else {
                intermediate <- performance
            }
        }
    }
}

# Collect and export results
if (length(intermediate) == 0) {
    output <- collect_stats(conn, info)
} else {
    output <- combine_stats(intermediate, info)
}
write(toJSON(output, auto_unbox=T),
      file=paste('output', arguments$filename, sep='/'))
