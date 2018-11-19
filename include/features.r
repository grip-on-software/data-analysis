# Utilities for retrieving sprint features.

library(jsonlite)
library(plyr)
library(yaml)
source('include/database.r')
source('include/log.r')
source('include/project.r')

safe_unbox <- function(x) {
    if (is.vector(x) && length(x) > 1) {
        return(x)
    }
    if (is.vector(x) && length(x) == 0) {
        return(NA)
    }
    if (is.data.frame(x) && nrow(x) == 0) {
        return(NA)
    }
    if (is.list(x)) {
        return(sapply(x, safe_unbox, simplify=F))
    }
    return(unbox(x))
}

get_locales <- function(items) {
    locales <- list()
    for (type in names(items)) {
        item <- items[[type]]
        for (code in names(item)) {
            if (!(code %in% names(locales))) {
                locales[[code]] <- list()
            }
            locales[[code]][[type]] <- safe_unbox(item[[code]])
        }
    }
    return(locales)
}

get_feature_locales <- function(items, field='descriptions') {
    locales <- list()
    for (item in items) {
        for (code in names(item[[field]])) {
            if (!(code %in% names(locales))) {
                locales[[code]] <- list()
            }
            locales[[code]] <- c(locales[[code]],
                                 mapply(function(column, description) {
                                            safe_unbox(description)
                                        },
                                        item$column, item[[field]][[code]],
                                        SIMPLIFY=F))
        }
    }
    return(locales)
}

get_combined_features <- function(items, data, colnames, details, join_cols,
                                  combine=T) {
    new_data <- data.frame()
    if (isTRUE(combine)) {
        combine <- 10
    }

    if (is.character(combine)) {
        sprint_data <- data[, c('project_id', combine)]
        sprint_data[[combine]] <- as.Date(sprint_data[[combine]])
        duplicates <- duplicated(sprint_data)
        n <- length(which(!duplicates))
        new_data <- data.frame(project_id=data[!duplicates, 'project_id'],
                               sprint_count=rep(1, n))
        new_data[, colnames] <- data[!duplicates, colnames]
        indexes <- which(duplicates)
        if (length(indexes) == 0) {
            start <- c()
            end <- c()
        }
        else if (length(indexes) == 1) {
            start <- indexes - 1
            end <- indexes
        }
        else {
            start <- indexes[c(0, diff(indexes)) != 1]
            lagged <- embed(indexes, 2)
            diffs <- lagged[lagged[, 1] != lagged[, 2] + 1, ]
            start <- c(indexes[1] - 1, diffs[, 1] - 1)
            end <- c(diffs[, 2], indexes[length(indexes)])
        }
        row_num <- start[1]
        for (i in 1:length(start)) {
            if (start[i] != end[i]) {
                result <- update_combine_interval(items, data, new_data,
                                                  row_num, details, colnames,
                                                  c(start[i], end[i]))
                new_data[row_num, result$columns] <- result$row
                details <- result$details
            }
            row_num <- row_num + start[i+1] - end[i]
        }
    }
    else {
        colnames <- c(colnames, 'sprint_start', 'sprint_end')
        projects <- factor(data[, join_cols[[1]]])
        project_data <- split(data, projects)
        intervals <- lapply(project_data, function(project) {
            end <- nrow(project)
            combn(round(seq(1, end, length.out=max(2, min(combine, end)))), 2)
        })
        n <- sum(as.numeric(lapply(intervals,
                                   function(interval) { ncol(interval) })))
        new_data <- data.frame(project_id=rep(NA, n), sprint_count=rep(1, n))
        new_data[, colnames] <- rep(list(rep(NA, n)), length(colnames))

        i <- 0
        for (project in levels(projects)) {
            count <- ncol(intervals[[project]])
            new_data[seq(i, i+count), 'project_id'] <- as.numeric(project)
            old_data <- project_data[[project]]
            for (interval in 1:count) {
                i <- i + 1
                project_interval <- intervals[[project]][, interval]
                new_data[i, 'sprint_start'] <- project_interval[1]
                new_data[i, 'sprint_end'] <- project_interval[2]
                result <- update_combine_interval(items, old_data, new_data, i,
                                                  details, colnames,
                                                  project_interval)
                new_data[i, result$columns] <- result$row
                details <- result$details
            }
        }
    }
    return(list(data=new_data, colnames=colnames, details=details, items=items))
}

update_combine_interval <- function(items, old_data, data, row_num, details,
                                    colnames, interval) {
    range <- seq(interval[1], interval[2])
    project_id <- old_data[range[1], 'project_id']
    result <- list(row=data.frame(sprint_count=length(range)),
                   columns=c("sprint_count"))
    for (item in items) {
        cols <- item$column %in% colnames
        if (any(cols)) {
            combine <- function(column, combiner, expression=F, window=NULL) {
                column_data <- old_data[range, column]
                if (column %in% colnames && !all(is.na(column_data))) {
                    if (isTRUE(combiner)) {
                        return(eval(parse(text=expression), data[row_num, ]))
                    }
                    if (is.list(combiner)) {
                        cur_value <- column_data[1]
                        add_data <- old_data[range, combiner$add]
                        if (!is.null(window)) {
                            extra <- data[seq(row_num - window$dimension,
                                              row_num - 1), ]
                            size <- sum(extra[extra$project_id == project_id,
                                              'sprint_count'] - 1)
                            if (size > 0) {
                                cur_value <- 0
                                add_data <- c(cur_value, add_data,
                                              old_data[seq(range[1] - size - 1,
                                                           range[1] - 1),
                                                       combiner$add])
                            }
                        }
                        return(cur_value +
                               combiner$factor * sum(add_data[-1], na.rm=T))
                    }
                    return(do.call(combiner,
                                   c(list(column_data), list(na.rm=T))))
                }
                data.frame(NA)
            }
            if (!is.character(item$combine)) {
                combined <- mapply(combine, item$column,
                                   MoreArgs=list(item$combine,
                                                 expression=item$expression,
                                                 window=item$window))
            }
            else {
                combined <- mapply(combine, item$column, item$combine)
            }
            result$row[, item$column[cols]] <- combined[cols]
            result$columns <- c(result$columns, item$column[cols])
            if (!is.null(item$summarize) && !is.null(item$summarize$details) &&
                is.list(details) && !is.null(details[[item$column[1]]])) {
                feature <- details[[item$column[1]]]
                sprint_ids <- old_data[range[-1], 'sprint_id']
                sprint_id <- old_data[range[1], 'sprint_id']
                detail_name <- paste(project_id, sprint_id, sep=".")
                detail_names <- paste(project_id, sprint_ids, sep=".")
                if (is.null(feature[[detail_name]])) {
                    current <- data.frame(project_id=project_id,
                                          sprint_id=sprint_id)
                }
                else {
                    current <- feature[[detail_name]]
                }
                for (d in item$summarize$details) {
                    if (!(d %in% colnames(current))) {
                        current[[d]] <- list(list())
                    }
                    current[[d]][[1]] <- c(current[[d]][[1]],
                                           do.call("c",
                                                   lapply(feature[detail_names],

                                                          function(detail) {
                                                              detail[[d]][[1]]
                                                          })))
                }
                details[[item$column[1]]][detail_names] <- NULL
                details[[item$column[1]]][[detail_name]] <- current
            }
        }
    }

    meta_columns <- c('sprint_name', 'start_date', 'close_date')
    if (all(meta_columns %in% colnames)) {
        result$row$sprint_name <- paste(old_data[range, 'sprint_name'],
                                        collapse=", ")
        result$row$start_date <- min(old_data[range, 'start_date'])
        result$row$close_date <- max(old_data[range, 'close_date'])
        result$columns <- c(result$columns, meta_columns)
    }

    result$details <- details
    return(result)
}

get_features <- function(conn, features, exclude, items, data, colnames,
                         join_cols, details=F, required=c()) {
    if (length(features) == 1) {
        if (is.na(features)) {
            features <- unlist(sapply(items, function(item) { item$column }))
        }
        else {
            features <- unique(c(required, strsplit(features, ",")[[1]]))
        }
    }
    if (isTRUE(details)) {
        details <- list()
    }
    for (item in items) {
        if (all(item$column %in% features) &&
            length(grep(exclude, item$table)) == 0) {
            if (!is.null(item$result)) {
                result <- item$result
            }
            else if (!is.null(item$expression)) {
                expression <- parse(text=item$expression)
                result <- data.frame(data[join_cols])
                if (!is.null(item$window)) {
                    if (length(item$window$group) == 1) {
                        group_cols <- list(factor(data[, item$window$group]))
                        names(group_cols) <- item$window$group
                    }
                    else {
                        group_cols <- lapply(data[, item$window$group], factor)
                    }
                    groups <- split(data, as.list(group_cols), drop=T)
                    all <- do.call("c", lapply(groups, function(group) {
                        c(rep(item$default, item$window$dimension - 1),
                          eval(expression, group))
                    }))
                    result[item$column] <- all
                }
                else {
                    result[item$column] <- eval(expression, data)
                }
            }
            else if (is.null(item$query)) {
                stop(paste('No query or result available for', item$column))
            }
            else {
                loginfo('Executing query for table %s', item$table)
                time <- system.time(result <- dbGetQuery(conn, item$query))
                loginfo('Query for table %s took %f seconds', item$table,
                        time['elapsed'])
            }
            if (!is.null(item$summarize)) {
                summarize <- item$summarize
                group_names <- summarize$group
                group_cols <- lapply(result[, group_names], factor)
                groups <- split(result, as.list(group_cols), drop=T)
                result <- do.call("rbind", lapply(groups, function(group) {
                    group_result <- data.frame(group[1, group_names])
                    summarizer <- function(operation, field) {
                        do.call(operation, c(list(group[, field]),
                                             list(na.rm=T)))
                    }
                    group_result[, item$column] <- mapply(summarizer,
                                                          summarize$operation,
                                                          summarize$field)
                    return(group_result)
                }))

                if (is.list(details)) {
                    detailer <- function(group) {
                        group_details <- data.frame(group[1, group_names])
                        details <- lapply(group[, summarize$details],
                                          function(detail) { list(detail) })
                        group_details[, summarize$details] <- details
                        return(group_details)
                    }
                    details[[item$column[1]]] <- lapply(groups, detailer)
                }
            }
            data <- join(data, result, by=join_cols, type="left", match="first")
            if (!is.null(item$default)) {
                for (column in item$column) {
                    if (!(column %in% names(data))) {
                        logwarn(paste('Column', column, 'could not be found'))
                    }
                    else if (length(data[[column]]) == 0) {
                        logwarn(paste('Column', column, 'is empty'))
                    }
                    else {
                        data[is.na(data[[column]]), column] <- item$default
                    }
                }
            }
            colnames <- c(colnames, item$column)
        }
    }
    list(data=data, details=details, colnames=unique(colnames), items=items)
}

get_sprint_conditions <- function(latest_date='', core=F, sprint_days=NA,
                                  sprint_patch=NA, future=T) {
    conditions <- list()
    if (!missing(latest_date) && latest_date != '') {
        conditions <- c(conditions,
                        paste('${t("sprint")}.start_date <= CAST(\'',
                              latest_date, '\' AS TIMESTAMP)', sep=''))
    }
    if (core) {
        conditions <- c(conditions, 'COALESCE(is_support_team, false) = false',
                        'main_project IS NULL')
    }
    if (!is.na(sprint_days)) {
        conditions <- c(conditions,
                        paste("${sprint_close} - sprint.start_date >",
                              "interval '${sprint_days}' day"))
    }
    if (!is.na(sprint_patch)) {
        conditions <- c(conditions, ifelse(sprint_patch, '${s(sprint_patch)}',
                                           'NOT (${s(sprint_patch)})'))
    }
    if (!future) {
        conditions <- c(conditions, 'start_date IS NOT NULL',
                        'end_date IS NOT NULL')
    }
    return(conditions)
}

get_sprint_features <- function(conn, features, exclude, variables, latest_date,
                                core=F, sprint_days=NA, sprint_patch=NA,
                                future=T, combine=F, details=F, time=F) {
    conditions <- get_sprint_conditions(latest_date, core, sprint_days,
                                        sprint_patch)
    if (length(conditions) != 0) {
        where_clause <- paste('WHERE', paste(conditions, collapse=' AND '))
        sprint_conditions <- paste('AND', paste(conditions, collapse=' AND '))
    }
    else {
        where_clause <- ''
        sprint_conditions <- ''
    }
    patterns <- load_definitions('sprint_definitions.yml',
                                 list(sprint_days=sprint_days))

    fields <- c('sprint.project_id', 'sprint.sprint_id')
    colnames <- c("project_id", "sprint_num")
    if (time) {
        fields <- c(fields,
                    paste('CAST(${sprint_open} AS DATE) -',
                          'date \'1970-01-01\' AS time'))
        colnames <- c(colnames, 'time')
    }
    query <- paste('SELECT', paste(fields, collapse=', '),
                   'FROM gros.sprint
                    JOIN gros.project
                    ON project.project_id = sprint.project_id',
                   where_clause,
                   'ORDER BY sprint.project_id, ${sprint_open}, sprint.name')
    sprint_query <- load_query(list(query=query),
                               patterns)

    sprint_data <- dbGetQuery(conn, sprint_query$query)

    items <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
                          c(variables,
                            list(sprint_conditions=sprint_conditions)))
    join_cols <- c("project_id", "sprint_id")

    result <- get_features(conn, features, exclude, items, sprint_data,
                           colnames, join_cols, details=details,
                           required=c("sprint_num"))
    if (!identical(combine, F)) {
        return(get_combined_features(result$items, result$data,
                                     result$colnames, result$details,
                                     join_cols, combine=combine))
    }
    return(result)
}

get_recent_sprint_features <- function(conn, features, date, limit=5, closed=T,
                                       sprint_meta=c(), sprint_conditions='',
                                       project_fields=c('project_id'),
                                       project_meta=list(), old=F, details=F,
                                       combine=F, prediction='') {
    patterns <- load_definitions('sprint_definitions.yml')
    if (!missing(date)) {
        project_meta$recent <- date
    }
    projects <- get_projects_meta(conn, fields=project_fields,
                                  metadata=c(project_meta,
                                             list(main=T, recent=T)))
    projects <- projects[projects$main, ]
    if (!old) {
        projects <- projects[projects$recent, ]
    }

    if (closed) {
        sprint_conditions <- paste(sprint_conditions,
                                   'AND ${sprint_close} < CURRENT_TIMESTAMP()')
    }
    query <- 'SELECT sprint.project_id, project.name AS project_name,
              project.quality_display_name, project.quality_name,
              sprint.sprint_id, sprint.name AS sprint_name,
              sprint.start_date, ${sprint_close} AS close_date,
              sprint.board_id, ${old} AS "old"
              FROM gros.sprint
              JOIN gros.project
              ON project.project_id = sprint.project_id
              WHERE sprint.project_id = ${project_id}
              AND sprint.start_date IS NOT NULL
              ${s(sprint_conditions)}
              ORDER BY sprint.project_id, ${sprint_open} DESC, sprint.name DESC
              ${pager} ${limit}'
    sprint_data <- data.frame()
    variables <- c(patterns, list(sprint_conditions=sprint_conditions,
                                  limit=limit))
    for (project in projects$project_id) {

        item <- load_query(list(query=query),
                           c(variables, list(project_id=project,
                                             old='FALSE',
                                             pager='LIMIT')))
        sprint_data <- rbind(sprint_data, dbGetQuery(conn, item$query))

        if (old) {
            item <- load_query(list(query=query),
                               c(variables, list(project_id=project,
                                                 old='TRUE',
                                                 pager='OFFSET')))
            sprint_data <- rbind(sprint_data, dbGetQuery(conn, item$query))
        }
    }
    sprint_data$start_date <- as.POSIXct(sprint_data$start_date)
    sprint_data$close_date <- as.POSIXct(sprint_data$close_date)
    sprint_data <- arrange(sprint_data, sprint_data$project_id,
                           sprint_data$start_date, sprint_data$sprint_name)

    data <- yaml.load_file('sprint_features.yml')
    items <- list()
    colnames <- c("project_name", "quality_display_name", "quality_name",
                  "board_id", sprint_meta, "old")
    join_cols <- c("project_id", "sprint_id")
    for (item in data$files) {
        if (all(item$column %in% features)) {
            items <- c(items, list(load_query(item, variables, data$path)))
        }
    }

    result <- get_features(conn, features, '^$', items, sprint_data, colnames,
                           join_cols, details=details, required=c("sprint_num"))

    if (prediction != '') {
        loginfo('Collecting predictions from %s', prediction)
        data <- fromJSON(url(prediction))
        predictions <- do.call("rbind",
                               mapply(function(labels, projects, sprints) {
                                          data.frame(project_id=projects,
                                                     sprint_num=sprints,
                                                     prediction=labels)
                               },
                               data$labels, data$projects, data$sprints,
                               SIMPLIFY=F, USE.NAMES=F))
        result$data <- join(result$data, predictions,
                            by=c("project_id", "sprint_num"), type="left",
                            match="first")
        result$items <- c(result$items,
                          list(list(column="prediction",
                                    combine="mean",
                                    descriptions=list(nl="Voorspelling",
                                                      en="Prediction"))))
        result$colnames <- c(result$colnames, "prediction")
    }
    if (!identical(combine, F)) {
        result <- get_combined_features(result$items, result$data,
                                        result$colnames, result$details,
                                        join_cols, combine=combine)
    }
    result$projects <- projects
    return(result)
}

get_project_features <- function(conn, features, exclude, variables, core=F) {
    if (core) {
        data <- get_core_projects(conn, by='name')
    }
    else {
        data <- get_projects(conn, by='name')
    }

    items <- load_queries('project_features.yml', 'sprint_definitions.yml',
                          variables)
    colnames <- c()
    join_cols <- c("project_id")
    get_features(conn, features, exclude, items, data, colnames, join_cols)
}

write_feature_metadata <- function(projects, specifications, output_directory,
                                   features=c(), items=c()) {
    if (length(items) == 0) {
        items <- specifications$files
    }
    write(toJSON(get_feature_locales(items)),
          file=paste(output_directory, "descriptions.json", sep="/"))
    write(toJSON(get_feature_locales(items, 'units')),
          file=paste(output_directory, "units.json", sep="/"))
    write(toJSON(get_feature_locales(items, 'short_units')),
          file=paste(output_directory, "short_units.json", sep="/"))
    write(toJSON(get_feature_locales(items, 'tags')),
          file=paste(output_directory, "tags.json", sep="/"))
    write(toJSON(get_locales(yaml.load_file("source_types.yml"))),
          file=paste(output_directory, "sources.json", sep="/"))
    write(toJSON(projects, auto_unbox=T),
          file=paste(output_directory, "projects.json", sep="/"))

    if (length(features) > 0) {
        cats <- specifications$categories

        for (item in items) {
            feature <- item$column[item$column %in% features]
            cat <- ifelse("category" %in% names(item), item$category, "other")
            if (length(feature) > 0) {
                cats[[cat]]$items <- c(cats[[cat]]$items, feature)
            }
        }
        categories <- mapply(function(cat, name) {
                                 if (name == "other") {
                                     cat$nl <- "Overig"
                                     cat$en <- "Other"
                                     cat$icon <- c("fas", "fa-ellipsis-h")
                                 }
                                 cat$name <- name
                                 cat$items <- I(cat$items)
                                 return(cat)
                             },
                             cats, names(cats), SIMPLIFY=F, USE.NAMES=F)
        write(toJSON(categories, auto_unbox=T),
              file=paste(output_directory, "categories.json", sep="/"))
    }
}
