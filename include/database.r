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
    organization <- NULL
    get_config <- function() {
        if (is.null(config)) {
            config_file <- get_arg('--config', default='config.yml')
            organization <<- get_arg('--org',
                                     default=Sys.getenv("ANALYSIS_ORGANIZATION"))
            config <<- yaml.load_file(config_file)
            if (!is.null(config[[organization]])) {
                config <<- config[[organization]]
                if (!is.null(config$arguments)) {
                    add_args(config$arguments)
                }
            }
        }
        return(config)
    }

    connect <- function() {
        config <- get_config()
        dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
                  user=config$db$user, password=config$db$password)
    }

    format_aliases <- function(fields) {
        return(mapply(function(alias, expression) {
                          paste(expression, ' AS "', alias, '"', sep='')
                      },
                      names(fields), fields))
    }

    get_primary_tables <- function() {
        sources <- list(jira=list(issue="issue",
                                  sprint="sprint",
                                  project="project",
                                  component="component",
                                  developer="developer"),
                        jira_component_version=list(issue="issue",
                                                    sprint="sprint",
                                                    project="project",
                                                    component="fixversion",
                                                    developer="developer"),
                        jira_version=list(issue="issue",
                                          sprint="fixversion",
                                          project="project",
                                          component="component",
                                          developer="developer"),
                        tfs=list(issue="tfs_work_item",
                                 sprint="tfs_sprint",
                                 project="tfs_team",
                                 developer="tfs_developer"))
        return(sources[[config$db$primary_source]])
    }

    load_definitions <- function(definition_file, variables=NULL,
                                 current_time=Sys.time()) {
        definitions <- yaml.load_file(definition_file)
        for (name in names(variables)) {
            arg <- get_arg(paste('--', gsub('_', '-', name), sep=''),
                           default=variables[[name]])
            variables[[name]] <- arg
        }
        primary_tables <- get_primary_tables()
        variables <- c(variables, primary_tables)

        has_aliasing <- function(alias, table) {
            return(isTRUE(alias) ||
                   (is.na(alias) &&
                    (is.null(table) || table %in% names(primary_tables))))
        }
        get_define <- function(define, field) {
            if (!is.null(define[[config$db$primary_source]])) {
                return(define[[config$db$primary_source]][[field]])
            }
            if (!is.null(define$jira)) {
                return(define$jira[[field]])
            }
            return(define[[field]])
        }
        issue_next_changelog <- list(left=c("issue_id", "changelog_id"),
                                     right=c("issue_id", "changelog_id + 1"))
        patterns <- c(lapply(definitions$fields, get_define, "field"),
                      lapply(definitions$conditions, get_define, "condition"),
                      list(issue_next_changelog=issue_next_changelog,
                           issue_changelog=issue_next_changelog$left),
                      variables,
                      list(join_cols=c("project_id", "sprint_id"),
                           issue_join='', component_join='', source='',
                           project_condition='', filter_condition='',
                           filter_inverse='', cond_op='AND', project_ids='0',
                           current_timestamp=paste("CAST('", current_time,
                                                   "' AS TIMESTAMP)", sep="")))
        recursive_str_interp <- function(string, ...) {
            vars <- c(..., as.list(parent.frame()), patterns)
            if (is.list(string)) {
                if (!is.null(vars$source) && !is.null(string[[vars$source]])) {
                    string <- string[[vars$source]]
                }
                else {
                    string <- ''
                }
            }
            return(str_interp(string, vars))
        }
        var_str_interp <- function(variable, ...) {
            vars <- c(..., as.list(parent.frame()), variables)
            if (!is.null(vars[[variable]])) {
                variable <- vars[[variable]]
            }
            if (variable %in% names(primary_tables)) {
                variable <- primary_tables[[variable]]
            }
            return(variable)
        }
        field_str_interp <- function(field, table=NULL, mask=T, alias=NA, ...) {
            extra_fields <- c()
            primary_table <- NULL
            var <- c(..., as.list(parent.frame()), variables)
            if (is.null(table)) {
                if (!is.null(definitions$fields[[field]])) {
                    definition <- definitions$fields[[field]]
                }
                else {
                    definition <- definitions$conditions[[field]]
                }
                table <- get_define(definition, "table")
                if (length(table) > 1) {
                    table <- table[table %in% primary_tables][[1]]
                }
                if (table %in% primary_tables) {
                    table <- names(primary_tables)[table == primary_tables]
                }
                var_table <- var_str_interp(table, var)
                field <- get_define(definition, "column")
            }
            else {
                if (table %in% names(var) &&
                    var[[table]] %in% names(primary_tables)) {
                    table <- var[[table]]
                    primary_table <- primary_tables[[var[[table]]]]
                }
                else if (table %in% names(primary_tables)) {
                    primary_table <- primary_tables[[table]]
                }
                var_table <- var_str_interp(table, var)

                if (is.list(field)) {
                    if (!is.null(var$source) && !is.null(field[[var$source]])) {
                        extra_fields <- field[[var$source]]
                        if (has_aliasing(alias, table)) {
                            extra_fields <- format_aliases(extra_fields)
                        }
                        else if (is.na(alias) || is.null(primary_table)) {
                            extra_fields <- paste(var_table,
                                                  names(extra_fields), sep=".")
                        }
                        else if (!identical(alias, F)) {
                            extra_fields <- names(extra_fields)
                        }
                    }
                    field <- field$default
                }
            }
            primary_id <- field == primary_table
            if (var_table == "") {
                fields <- field
            }
            else {
                fields <- paste(var_table, field, sep=".")
            }
            if (any(primary_id)) {
                if (is.character(alias) && alias == "alias") {
                    fields[primary_id] <- paste(var_table, "id", sep=".")
                }
                else if (has_aliasing(alias, table)) {
                    fields[primary_id] <- paste(paste(var_table, "id", sep="."),
                                                "AS", field[primary_id])
                }
                else if (!identical(alias, F)) {
                    fields[primary_id] <- field[primary_id]
                }
            }
            all_fields <- c(fields, extra_fields)
            return(paste(all_fields[mask], collapse=", "))
        }
        group_str_interp <- function(field, table, extra="", mask=T, ...) {
            if (extra != "") {
                extra <- paste(",", extra)
            }
            vars <- c(..., as.list(parent.frame()), variables)
            fields <- field_str_interp(field, table, mask=mask, alias="alias",
                                       vars)
            return(paste('GROUP BY ', fields, extra, sep=""))
        }
        join_str_interp <- function(field, left, right, mask=T, source=NULL,
                                    var=F, ...) {
            if (is.list(field) && !is.null(field$default)) {
                fields <- field$default
            }
            else {
                fields <- field
            }
            fields <- fields[mask]

            if (is.list(fields)) {
                left_fields <- fields$left
                right_fields <- fields$right
            }
            else {
                left_fields <- fields
                right_fields <- fields
            }
            vars <- c(..., as.list(parent.frame()), variables)
            left_table <- var_str_interp(left, vars)
            right_table <- var_str_interp(right, vars)
            left_fields[left_fields == left_table] <- "id"
            right_fields[right_fields == right_table] <- "id"

            left <- left_fields %in% names(vars)
            if (var && any(left) && is.list(vars[[left_fields[left]]])) {
                left_fields[left] <- vars[[left_fields[left]]][[left_table]]
            }
            right <- right_fields %in% names(vars)
            if (var && any(right) && is.list(vars[[right_fields[right]]])) {
                right_fields[right] <-
                    vars[[right_fields[right]]][[right_table]]
            }

            joins <- paste(paste(left_table, left_fields, sep="."),
                           paste(right_table, right_fields, sep="."),
                           sep=" = ", collapse=" AND ")
            if (!is.null(source) && is.list(field) &&
                !is.null(field[[source]])) {
                extra <- names(field[[source]])
                joins <- paste(joins, "AND",
                               paste(paste("COALESCE(", left_table, ".", extra,
                                           ", '')", sep=""),
                                     paste("COALESCE(", right_table, ".", extra,
                                           ", '')", sep=""),
                                     sep=" = ", collapse=" AND "))
            }
            return(joins)
        }
        query_str_interp <- function(string, ...) { sub("\\S+\\.", "", string) }
        patterns <- c(patterns, list(s=recursive_str_interp,
                                     f=field_str_interp,
                                     g=group_str_interp,
                                     j=join_str_interp,
                                     q=query_str_interp,
                                     t=var_str_interp))

        return(patterns)
    }

    load_query <- function(item, patterns, path) {
        if (!is.null(item$definition)) {
            fields <- list(paste('${f(join_cols, "', item$table, '")}', sep=""))
            define <- patterns[[item$definition]]
            fields <- c(fields, paste(define, "AS", item$column, sep=" "))
            item$query <- paste('SELECT', paste(fields, collapse=", "),
                                'FROM', paste('gros.${t("', item$table, '")}',
                                              sep=''),
                                paste('${s(component_join, project=t("',
                                      item$table, '"))}', sep=''),
                                'WHERE 1=1',
                                paste('${s(project_condition, project=t("',
                                      item$table, '"))}', sep='')
            )
        }
        else if (!is.null(item$metric)) {
            columns <- c('${f(join_cols, "metric_value")}')
            if (is.null(item$source)) {
                metric_history <- '${metric_history_url}/${metric_history_file}'
                item$source <- list(quality_time='${quality_time_url}',
                                    quality='${quality_url}/${quality_name}',
                                    metric_history=metric_history)
                item$source[item$backends] <- list(NULL)
            }
            item$cache <- TRUE
            if (!is.null(item$summarize)) {
                columns <- c(columns, 'metric.domain_name')
                field <- paste(item$aggregate, 'value', sep="_")
                item$summarize <- list(operation=item$summarize,
                                       field=field,
                                       group=c('project_id', 'sprint_id'),
                                       component='domain_name',
                                       key='domain_name',
                                       details=c('domain_name', field))
            }
            else {
                field <- item$column
            }

            table <- sub("(.)([A-Z][a-z]+)", "\\1_\\2", item$metric[1])
            table <- tolower(sub("([a-z0-9])([A-Z])", "\\1_\\2", table))
            table <- paste("metric", table, sep="_")
            if (item$aggregate == "end") {
                columns <- c(columns, 'metric_value.value')
                colnames <- sub("^.*\\.", "", columns[-c(1, length(columns))])
                table_cols <- c(paste('${f(join_cols, "', table, '")}', sep=''),
                                colnames)
                cols <- paste(c('${f(join_cols, "metric_value")}', colnames),
                                  collapse=",")

                aggregate <- 'MAX(metric_value.date)'
                aggregate <- c(aggregate,
                               paste('ROW_NUMBER() OVER (
                                          PARTITION BY', cols, 'ORDER BY',
                                          cols, ',', aggregate, 'DESC
                                      )'))
                aggregate_fields <- c('end_date', 'rev_row')
                wrap_query <- paste('SELECT', paste(table_cols, collapse=","),
                                    ', MAX(value) AS', field,
                                    'FROM ($query) AS', table,
                                    'WHERE rev_row = 1
                                     GROUP BY', paste(table_cols, collapse=","))
            }
            else {
                aggregate <- paste(toupper(item$aggregate), "(value)", sep="")
                aggregate_fields <- field
            }
            item$table <- table
            item$category <- "metrics"
            if (!("combine" %in% names(item))) {
                item$combine <- list(project="sum", sprint="end")
            }
            item$query <- paste('SELECT', paste(columns, collapse=","), ",",
                                paste(aggregate, aggregate_fields,
                                      sep=" AS ", collapse=", "),
                                'FROM gros.metric_value
                                 JOIN gros.metric
                                 ON metric_value.metric_id = metric.metric_id
                                 WHERE metric.base_name IN (',
                                 paste('\'', item$metric, '\'',
                                       sep="", collapse=","),
                                ') AND metric.domain_name NOT IN (',
                                 paste('\'', c('', config$exclude_domain), '\'',
                                       sep="", collapse=","),
                                ') AND metric_value.sprint_id <> 0
                                 AND metric_value.value > -1
                                 ${s(project_condition, project="metric_value")}
                                 GROUP BY', paste(columns, collapse=","))

            if (item$aggregate == "end") {
                item$query <- str_replace(wrap_query, '\\$query', item$query)
            }
        }
        else if (!is.null(item$filename)) {
            path <- paste(path, item$filename, sep="/")
            item$query <- paste(readLines(path, encoding="UTF-8"),
                                collapse="\n")
        }

        if (!is.null(item$query)) {
            source <- config$db$primary_source
            item$patterns <- c(item$patterns,
                               lapply(item$fields,
                                      function(field) {
                                          if (!is.null(field[[source]])) {
                                              return(field[[source]])
                                          }
                                          return(field)
                                      }),
                               patterns)
            item$patterns$source <- names(item$source)[1]
            if (!is.null(item$patterns$source) &&
                item$patterns$source %in% c("jira", "tfs") &&
                source != "jira_version") {
                item$patterns$source <- source
            }
            item$query <- str_interp(item$query, item$patterns)
        }
        if (!is.null(item$summarize)) {
            def <- list(with_missing=rep(F, length(item$summarize$operation)),
                        reference=rep(NA, length(item$summarize$operation)),
                        filter='TRUE',
                        key='key')
            item$summarize <- modifyList(def, item$summarize)
        }
        return(item)
    }

    load_queries <- function(specification_file, definition_file=NULL,
                             variables=NULL, current_time=Sys.time()) {
        data <- yaml.load_file(specification_file)
        if (is.null(definition_file)) {
            patterns <- variables
        }
        else {
            patterns <- load_definitions(definition_file, variables,
                                         current_time=current_time)
        }

        lapply(data$files, load_query, patterns, data$path)
    }
}
