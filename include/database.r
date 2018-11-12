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
            config <<- yaml.load_file(config_file)
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

        patterns <- c(lapply(definitions$fields,
                               function(define) { define$field }),
                      lapply(definitions$conditions,
                               function(define) { define$condition }),
                      variables)
        recursive_str_interp <- function(string, ...) {
            str_interp(string, c(as.list(parent.frame()), patterns, list(...)))
        }
        var_str_interp <- function(variable, ...) {
            variables <- c(as.list(parent.frame()), list(...))
            if (!is.null(variables[[variable]])) {
                return(variables[[variable]])
            }
            return(variable)
        }
        patterns <- c(patterns, list(s=recursive_str_interp,
                                     t=var_str_interp))

        return(patterns)
    }

    load_query <- function(item, patterns, path) {
        if (!is.null(item$definition)) {
            fields <- list()
            for (field in c("project_id", "sprint_id")) {
                fields <- c(fields, paste(item$table, field, sep="."))
            }
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
            item$table <- item$metric
            item$category <- "metrics"
            item$combine <- "sum"
            item$query <- paste('SELECT', paste(columns, collapse=","), ",",
                                paste(toupper(item$aggregate), "(value) AS ",
                                      field, collapse=", ", sep=""),
                                'FROM gros.metric_value
                                 JOIN gros.metric
                                 ON metric_value.metric_id = metric.metric_id
                                 WHERE metric.base_name =',
                                 paste('\'', item$metric, '\'', sep=""),
                                'AND metric_value.sprint_id <> 0
                                 AND metric_value.value > -1
                                 GROUP BY', paste(columns, collapse=","))
        }
        else if (!is.null(item$filename)) {
            path <- paste(path, item$filename, sep="/")
            item$query <- paste(readLines(path, encoding="UTF-8"),
                                collapse="\n")
        }

        if (!is.null(item$query)) {
            item$query <- str_interp(item$query, patterns)
            item$patterns <- patterns
        }
        return(item)
    }

    load_queries <- function(specification_file, definition_file, variables) {
        data <- yaml.load_file(specification_file)
        patterns <- load_definitions(definition_file, variables)

        lapply(data$files, load_query, patterns, data$path)
    }
}
