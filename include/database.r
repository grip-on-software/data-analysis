# Utility file for functions that allow accessing and querying the database

if (!exists('INC_DATABASE_R')) {
    INC_DATABASE_R <- T

    library(MonetDB.R)
    library(DBI)
    library(digest)
    library(stringr)
    library(yaml)
    source('include/args.r')

    config <- NULL
    get_config <- function() {
        if (is.null(config)) {
            config_file <- get_arg('--config', default='config.yml')
            organization <- get_arg('--org',
                                    default=Sys.getenv("ANALYSIS_ORGANIZATION"))
            config <<- yaml.load_file(config_file)
            if (!is.null(config[[organization]])) {
                config <<- config[[organization]]
            }
        }
        return(config)
    }

    connect <- function() {
        config <- get_config()
        dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
                  user=config$db$user, password=config$db$password)
    }

    load_definitions <- function(definition_file, variables) {
        definitions <- yaml.load_file(definition_file)
        if (missing(variables)) {
            variables <- NULL
        }
        else {
            for (name in names(variables)) {
                arg <- get_arg(paste('--', gsub('_', '-', name), sep=''),
                               default=variables[[name]])
                variables[[name]] <- arg
            }
        }
        sources <- list(jira=list(issue="issue",
                                  sprint="sprint",
                                  project="project"),
                        tfs=list(issue="tfs_work_item",
                                 sprint="tfs_sprint",
                                 project="tfs_team"))
        variables <- c(variables, sources[[config$db$primary_source]])

        get_define <- function(define, field) {
            if (!is.null(define[[config$db$primary_source]])) {
                return(define[[config$db$primary_source]][[field]])
            }
            return(define[[field]])
        }
        issue_next_changelog <- list(left=c("issue_id", "changelog_id"),
                                     right=c("issue_id", "changelog_id + 1"))
        patterns <- c(lapply(definitions$fields, get_define, "field"),
                      lapply(definitions$conditions, get_define, "condition"),
                      list(issue_next_changelog=issue_next_changelog,
                           issue_changelog=issue_next_changelog$left),
                      variables)
        recursive_str_interp <- function(string, ...) {
            str_interp(string, c(as.list(parent.frame()), patterns, list(...)))
        }
        var_str_interp <- function(variable, ...) {
            vars <- c(variables, as.list(parent.frame()), list(...))
            if (!is.null(vars[[variable]])) {
                return(vars[[variable]])
            }
            return(variable)
        }
        field_str_interp <- function(field, table=NULL, ...) {
            if (is.null(table)) {
                if (!is.null(definitions$fields[[field]])) {
                    definition <- definitions$fields[[field]]
                }
                else {
                    definition <- definitions$conditions[[field]]
                }
                var_table <- get_define(definition, "table")
                if (length(var_table) > 1) {
                    tables <- var_table %in% sources[[config$db$primary_source]]
                    var_table <- var_table[as.character(tables)][1]
                }
                field <- get_define(definition, "column")
            }
            else {
                var_table <- var_str_interp(table, ...)
            }
            return(paste(var_table, field, sep=".", collapse=", "))
        }
        join_str_interp <- function(field, left, right, ...) {
            if (is.list(field)) {
                left_fields <- field$left
                right_fields <- field$right
            }
            else {
                left_fields <- field
                right_fields <- field
            }
            left_table <- var_str_interp(left, ...)
            right_table <- var_str_interp(right, ...)
            return(paste(paste(left_table, left_fields, sep="."),
                         paste(right_table, right_fields, sep="."),
                         sep=" = ", collapse=" AND "))
        }
        patterns <- c(patterns, list(s=recursive_str_interp,
                                     f=field_str_interp,
                                     j=join_str_interp,
                                     t=var_str_interp))

        return(patterns)
    }

    load_query <- function(item, patterns, path) {
        if (!is.null(item$definition)) {
            fields <- list(paste('${f(join_cols, "', item$table, '")}'))
            define <- patterns[[item$definition]]
            fields <- c(fields, paste(define, "AS", item$column, sep=" "))
            item$query <- paste('SELECT', paste(fields, collapse=", "),
                                'FROM', paste('gros', item$table, sep='.'))
        }
        else if (!is.null(item$metric)) {
            columns <- c('metric_value.project_id', 'metric_value.sprint_id')
            if (!is.null(item$summarize)) {
                columns <- c(columns, 'metric.domain_name')
                field <- paste(item$aggregate, 'value', sep="_")
                item$summarize <- list(operation=item$summarize,
                                       field=field,
                                       group=c('project_id', 'sprint_id'),
                                       details=c('domain_name', field))
            }
            else {
                field <- item$column
            }
            if (item$aggregate == "end") {
                columns <- c(columns, 'metric_value.value')
                aggregate <- 'MAX(metric_value.date) AS end_date'
            }
            else {
                aggregate <- paste(toupper(item$aggregate), "(value) AS ",
                                   field, collapse=", ", sep="")
            }
            item$table <- item$metric
            item$category <- "metrics"
            if (!("combine" %in% names(item))) {
                item$combine <- "sum"
            }
            item$query <- paste('SELECT', paste(columns, collapse=","), ",",
                                aggregate,
                                'FROM gros.metric_value
                                 JOIN gros.metric
                                 ON metric_value.metric_id = metric.metric_id
                                 WHERE metric.base_name =',
                                 paste('\'', item$metric, '\'', sep=""),
                                'AND metric_value.sprint_id <> 0
                                 AND metric_value.value > -1
                                 GROUP BY', paste(columns, collapse=","))

            if (item$aggregate == "end") {
                colnames <- paste(lapply(strsplit(columns[-length(columns)],
                                                  ".", fixed=TRUE),
                                         function(column) {
                                             column[[length(column)]]
                                         }),
                                  collapse=",")
                item$query <- paste('SELECT', colnames, ',',
                                    'MAX(value) AS', field,
                                    'FROM (
                                        SELECT', colnames, ', value,',
                                        'ROW_NUMBER() OVER (
                                            PARTITION BY', colnames, 'ORDER BY',
                                            colnames, ', end_date DESC
                                        ) AS rev_row
                                        FROM (', item$query, ') AS metric_rows
                                    ) AS ', item$metric,
                                    'WHERE rev_row = 1
                                     GROUP BY', paste(colnames, collapse=","))
            }
        }
        else if (!is.null(item$filename)) {
            path <- paste(path, item$filename, sep="/")
            item$query <- paste(readLines(path, encoding="UTF-8"),
                                collapse="\n")
        }

        if (!is.null(item$query)) {
            item$patterns <- c(item$patterns, patterns)
            item$query <- str_interp(item$query, item$patterns)
        }
        return(item)
    }

    load_queries <- function(specification_file, definition_file, variables) {
        data <- yaml.load_file(specification_file)
        patterns <- load_definitions(definition_file, variables)

        lapply(data$files, load_query, patterns, data$path)
    }
}
