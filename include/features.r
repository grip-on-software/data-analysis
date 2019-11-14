# Utilities for retrieving sprint features.

library(jsonlite)
library(plyr)
library(yaml)
library(zoo)
library(CORElearn)
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

count <- function(data, na.rm=F) {
    if (na.rm) {
        return(length(which(!is.na(data))))
    }
    return(length(data))
}

count_unique <- function(data, na.rm=F) {
    return(length(which(!duplicated(data, incomparables=NA) &
                        (!na.rm | !is.na(data)))))
}

sum_of_na_avg <- function(data, reference=NULL, na.rm=F) {
    if (is.null(reference)) {
        reference <- data
    }
    sub <- sum(reference, na.rm=na.rm)
    return(sub / length(which(!is.na(reference))) * length(which(is.na(data))))
}

mode <- function(data, na.rm=F) {
    unique_data <- unique(data[!is.na(data)])
    return(unique_data[which.max(tabulate(match(data, unique_data)))])
}

end <- function(data, na.rm=F) {
    return(tail(data[!na.rm | !is.na(data)], n=1))
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
                                  combine=T, teams=list(), limit=5,
                                  date=T, main=T, projects=data.frame(),
                                  components=NULL) {
    if (isTRUE(combine)) {
        combine <- 10
    }
    team_projects <- list()
    if (!("project_id" %in% colnames)) {
        data$project_id <- data$team_id
    }
    data$original_project_id <- data$project_id
    projects$team <- T
    projects$component <- F

    if (!is.null(components)) {
        project_id <- max(data$project_id + 1)
        for (component in components) {
            condition <- data$project_name == component$project &
                !is.na(data$component) & data$component == component$name
            data[condition, "project_id"] <- project_id
            data[condition, "project_name"] <- component$name
            display_name <- ifelse(is.null(component$display_name),
                                   component$name, component$display_name)
            data[condition, "quality_display_name"] <- display_name

            project <- projects[projects$name == component$project, ]
            if (nrow(project) == 0) {
                next
            }
            metadata <- data.frame(project_id=project_id,
                                   project_names=project$project_names,
                                   name=component$name,
                                   quality_display_name=display_name,
                                   recent=project$recent,
                                   main=ifelse(is.null(component$main),
                                               project$main, component$main),
                                   core=project$core,
                                   team=F,
                                   component=T,
                                   stringsAsFactors=F)
            if (!is.null(component$jira)) {
                # Add primary source filter data into component data
                metadata$component <- list(list(component$jira))
            }
            metadata$project_ids <- project$project_ids
            if (!main || metadata$main) {
                metadata <- metadata[, colnames(projects)]
                if (component$name %in% projects$name) {
                    projects[projects$name == component$name, ] <- metadata
                }
                else {
                    projects <- rbind(projects, metadata)
                }
            }
            project_id <- project_id + 1
        }
    }

    if (is.character(combine)) {
        teams <- get_combined_teams(data, teams, date, projects, colnames)
        data <- teams$data
        projects <- teams$projects
        colnames <- teams$colnames
        duplicates <- teams$duplicates
        team_projects <- teams$team_projects

        sprint_data <- data[, c('project_name', combine)]
        sprint_data[[combine]] <- as.Date(sprint_data[[combine]])
        duplicates <- duplicates | (duplicated(sprint_data) & !data$future)

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
        if (!is.null(start)) {
            row_num <- start[1]
            for (i in 1:length(start)) {
                if (start[i] != end[i]) {
                    result <- update_combine_interval(items, data, new_data,
                                                      row_num, details,
                                                      colnames,
                                                      c(start[i], end[i]),
                                                      join_cols)
                    new_data[row_num, result$columns] <- result$row
                    details <- result$details
                }
                row_num <- row_num + start[i+1] - end[i]
            }
        }
    }
    else {
        colnames <- c(colnames, 'sprint_start', 'sprint_end')
        project_groups <- factor(data[, join_cols[[1]]])
        project_data <- split(data, project_groups)
        intervals <- lapply(project_data, function(project) {
            end <- nrow(project)
            combn(round(seq(1, end, length.out=max(2, min(combine, end)))), 2)
        })
        n <- sum(as.numeric(lapply(intervals,
                                   function(interval) { ncol(interval) })))
        new_data <- data.frame(project_id=rep(NA, n), sprint_count=rep(1, n))
        new_data[, colnames] <- rep(list(rep(NA, n)), length(colnames))

        i <- 0
        for (project in levels(project_groups)) {
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
                                                  project_interval, join_cols)
                new_data[i, result$columns] <- result$row
                details <- result$details
            }
        }
    }
    return(list(data=new_data, colnames=colnames, join_cols=join_cols,
                details=details, items=items,
                projects=projects, team_projects=team_projects))
}

get_combined_teams <- function(data, teams, date, projects, colnames) {
    recent_date <- get_recent_date(date)
    data$team_id <- data$project_id
    colnames <- c(colnames, "team_id")
    data$duplicate <- rep(F, nrow(data))
    result <- list(data=data, projects=projects, team_id=0,
                   team_projects=list())
    for (team in teams) {
        result <- get_combined_team(team, result$team_id - 1,
                                    result$data, result$projects,
                                    result$team_projects, recent_date)
    }
    projects <- projects[projects$name %in% data$project_name, ]
    result$data <- arrange(result$data, result$data$project_name,
                           result$data$start_date, result$data$sprint_name)

    duplicates <- result$data$duplicate
    result$data$duplicate <- NULL

    return(c(result, list(colnames=colnames, duplicates=duplicates)))
}

get_combined_team <- function(team, team_id, data, projects, team_projects,
                              recent_date) {
    project_names <- sapply(team$projects,
                            function(project) {
                                return(ifelse(is.list(project),
                                              project$key, project))
                            })

    team_projects[[team$name]] <- project_names
    team_conditions <- data$project_name %in% project_names
    replace <- c()
    for (project in team$projects) {
        if (is.list(project)) {
            if (!is.null(project$start_date)) {
                team_conditions <- team_conditions &
                    (data$project_name != project$key |
                     as.Date(data$start_date) >=
                     as.Date(project$start_date))
            }
            if (!is.null(project$end_date)) {
                team_conditions <- team_conditions &
                    (data$project_name != project$key |
                     as.Date(data$close_date) <=
                     as.Date(project$end_date))
            }
            if (!is.null(project$board)) {
                team_conditions <- team_conditions &
                    (data$project_name != project$key |
                     data$board_id %in% project$board)
            }
            if (isTRUE(project$replace)) {
                replace <- c(replace, project$key)
            }
        }
    }

    t <- length(which(team_conditions))
    if (t == 0) {
        return(list(data=data, projects=projects, team_id=team_id,
                    team_projects=team_projects))
    }
    loginfo("Team %s has %d unmerged sprints", team$name, t)
    display_name <- ifelse(is.null(team$display_name),
                           data[team_conditions, 'quality_display_name'][1],
                           team$display_name)
    team_meta <- list(team_id=rep(team_id, t),
                      project_name=rep(team$name, t),
                      quality_display_name=rep(display_name, t),
                      board_id=rep(team$board, t),
                      duplicate=rep(F, t))

    if (!is.null(team$overlap)) {
        sprint_data <- data[team_conditions, c('start_date', 'close_date')]
        if (nrow(sprint_data) > 0) {
            prev <- embed(as.matrix(sprint_data), 3)
            overlap <- as.Date(prev[, 4]) - as.Date(prev[, 1]) >= team$overlap &
                as.Date(prev[, 2]) >= as.Date(prev[, 4])
            for (index in which(overlap)) {
                if (overlap[index] && index < length(overlap)) {
                    overlap[index+1] <- F
                }
            }
            team_meta$duplicate <- c(F, F, overlap)
            loginfo('Team %s has %d overlapping sprints', team$name,
                    length(which(overlap)))
        }
    }

    team_meta <- team_meta[names(team_meta) %in% colnames(data)]
    team_data <- data[team_conditions, ]
    if (length(replace) > 0) {
        data[team_conditions, names(team_meta)] <- team_meta
        data <- data[!(data$project_name %in% replace) | team_conditions, ]
    }
    else {
        team_data[, names(team_meta)] <- team_meta
        data <- rbind(data, team_data)
    }

    meta_condition <- projects$name %in% project_names
    project_id <- unique(c(projects[meta_condition, 'project_id'],
                           unlist(projects[meta_condition, 'project_ids'])))
    core <- any(projects[meta_condition, 'core'])
    component <- list()
    for (components in projects[projects$name == team$name, 'component']) {
        if (is.list(components)) {
            component <- c(component, components[[1]])
        }
    }
    recent <- ifelse(!is.null(team$recent), team$recent,
                     any(as.Date(team_data[, 'start_date']) >= recent_date))

    metadata <- data.frame(project_id=team_id,
                           project_ids=0,
                           project_names=0,
                           name=team$name,
                           quality_display_name=display_name,
                           recent=recent,
                           main=T,
                           core=core,
                           team=ifelse(isTRUE(team$invisible), -1,
                                       ifelse(is.null(team$team), team$board,
                                              as.logical(team$team))),
                           component=F,
                           stringsAsFactors=F)[, colnames(projects)]
    metadata$project_ids <- list(project_id)
    metadata$project_names <- list(project_names)
    metadata$component <- ifelse(length(component) > 0,
                                 list(list(component)), F)

    if (team$name %in% projects$name) {
        existing_names <- projects[projects$name == team$name, "project_names"]
        if (isTRUE(team$names)) {
            metadata$project_names <- list(c(existing_names,
                                             metadata$project_names[[1]]))
        }
        else {
            metadata$project_names <- existing_names
        }
        projects[projects$name == team$name, ] <- as.list(metadata)
    }
    else {
        projects[meta_condition, 'team'] <- F
        if (length(replace) > 0) {
            projects <- projects[!(projects$name %in% replace), ]
        }
        projects <- rbind(projects, metadata)
    }

    if (length(team$projects) > 1) {
        for (project in team$projects) {
            if (is.list(project) && !is.null(project$board) &&
                !isTRUE(project$replace)
            ) {
                loginfo("Replacing project %s to clean up board", project$key)
                display_name <- projects[projects$name == project$key,
                                         'quality_display_name']
                if (length(display_name) == 0) {
                    display_name <- NA
                }
                board_project <- list(name=project$key,
                                      display_name=display_name,
                                      board=ifelse(isTRUE(project$own_board),
                                                   project$board[1],
                                                   team$board),
                                      team=F,
                                      projects=list(c(project,
                                                      list(replace=T))))
                result <- get_combined_team(board_project, team_id - 1,
                                            data, projects, team_projects,
                                            recent_date)
                data <- result$data
                projects <- result$projects
                team_projects <- result$team_projects
                team_id <- result$team_id
            }
        }
    }

    return(list(data=data, projects=projects, team_id=team_id,
                team_projects=team_projects))
}

update_combine_interval <- function(items, old_data, data, row_num, details,
                                    colnames, interval, join_cols) {
    range <- seq(interval[1], interval[2])
    project_col <- join_cols[1]
    sprint_col <- join_cols[2]
    num_projects <- length(unique(old_data[range, project_col]))
    result <- list(row=data.frame(sprint_count=length(range)),
                   columns=c("sprint_count"))
    for (item in items) {
        columns <- get_summarize_columns(item)
        columns <- columns[columns %in% colnames]

        if (length(columns) > 0) {
            if (is.list(item$combine)) {
                combiner <- ifelse(num_projects > 1, item$combine$project,
                                   item$combine$sprint)
            }
            else {
                combiner <- item$combine
            }
            combine <- function(column, combiner) {
                column_data <- old_data[range, column]
                if (!all(is.na(column_data))) {
                    return(do.call(combiner,
                                   c(list(column_data), list(na.rm=T))))
                }
                data.frame(NA)
            }
            combined <- mapply(combine, columns, combiner)
            result$row[, columns] <- combined
            result$columns <- c(result$columns, columns)

            if (!is.null(item$summarize) && !is.null(item$summarize$details) &&
                is.list(details) && !is.null(details[[item$column[1]]])) {
                feature <- details[[item$column[1]]]
                team_id <- old_data[range[1], 'team_id']
                sprint_id <- old_data[range[1], sprint_col]
                project_ids <- old_data[range, 'original_project_id']
                sprint_ids <- old_data[range, sprint_col]
                components <- old_data[range, 'component']
                if (!is.null(components)) {
                    detail_name <- paste(team_id, sprint_id, NA, sep=".")
                    detail_names <- unique(c(paste(project_ids,
                                                   sprint_ids,
                                                   components,
                                                   sep=".")))
                }
                else {
                    detail_name <- paste(team_id, sprint_id, sep=".")
                    detail_names <- unique(c(paste(project_ids,
                                                   sprint_ids,
                                                   sep=".")))
                }

                if (is.null(feature[[detail_name]])) {
                    row <- list()
                    row[[join_cols[1]]] <- team_id
                    row[[sprint_col]] <- sprint_id
                    row$component <- NA
                    current <- as.data.frame(row)
                }
                else {
                    current <- feature[[detail_name]]
                }
                for (d in item$summarize$details) {
                    if (!(d %in% colnames(current))) {
                        current[[d]] <- list(setNames(numeric(0), character(0)))
                    }
                    current[[d]][[1]] <- c(current[[d]][[1]],
                                           do.call("c",
                                                   lapply(feature[detail_names],
                                                          function(detail) {
                                                              detail[[d]][[1]]
                                                          })))
                }
                key <- item$summarize$key
                if (key %in% colnames(current)) {
                    duplicates <- duplicated(current[[key]][[1]])
                    for (d in item$summarize$details) {
                        current[[d]][[1]] <- current[[d]][[1]][!duplicates]
                    }
                }
                details[[item$column[1]]][[detail_name]] <- current

                if (!isTRUE(item$carry) && is.na(item$summarize$reference[1]) &&
                    item$summarize$field %in% colnames(current) &&
                    item$column[1] %in% result$columns) {
                    with_missing <- item$summarize$with_missing[1]
                    summarized <- do.call(item$summarize$operation[1],
                                          c(current[[item$summarize$field]],
                                            list(na.rm=!with_missing)))
                    result$row[, item$column[1]] <- summarized
                }
            }
        }
    }

    meta_columns <- c(sprint_col, 'sprint_name', 'board_id',
                      'start_date', 'close_date')
    if (all(meta_columns %in% colnames)) {
        result$row[[sprint_col]] <- list(unique(old_data[range, sprint_col]))
        result$row$sprint_name <- list(unique(old_data[range, 'sprint_name']))
        result$row$board_id <- list(unique(old_data[range, 'board_id']))
        result$row$start_date <- min(old_data[range, 'start_date'])
        result$row$close_date <- max(old_data[range, 'close_date'])
        result$columns <- c(result$columns, meta_columns)
    }

    result$details <- details
    return(result)
}

include_feature <- function(item, features, exclude, required=c()) {
    if (all(item$column %in% required)) {
        return(T)
    }
    if (all(item$column %in% features)) {
        if (!is.null(item$expression)) {
            return(length(grep(exclude, item$column)) == 0)
        }
        return(length(grep(exclude, item$table)) == 0)
    }
    return(F)
}

expand_feature_names <- function(feature, items, categories=list()) {
    features <- strsplit(feature, ",")[[1]]
    all <- unlist(lapply(items, function(item) { item$column }))
    sources <- unique(unlist(lapply(items,
                                    function(item) { names(item$source) })))

    filter_source <- function(item, feature) {
        if (!is.null(item$source) && feature %in% names(item$source)) {
            return(item$column)
        }
    }

    filter_category <- function(item, feature) {
        if (!is.null(item$category) && item$category == feature) {
            return(item$column)
        }
        if (feature == "metrics" && !is.null(item$metric)) {
            return(item$column)
        }
    }

    collect <- function(current, feature) {
        if (startsWith(feature, "-")) {
            exclude <- T
            feature <- substring(feature, 2)
        }
        else {
            exclude <- F
        }

        if (feature == "all") {
            feature <- all
        }
        else if (feature %in% sources) {
            feature <- lapply(items, filter_source, feature)
        }
        else if (feature %in% names(categories)) {
            feature <- lapply(items, filter_category,
                              feature)
        }

        if (exclude) {
            return(current[!(current %in% feature)])
        }
        return(c(current, feature))
    }

    features <- Reduce(collect, features, collect(c(), features[1]))
    return(unique(unlist(features)))
}

merge_features <- function(data, result, join_cols, components) {
    by <- join_cols
    match <- "first"
    if (!is.null(components)) {
        if ("component" %in% colnames(result)) {
            by <- c(by, "component")
            if ("original_component" %in% colnames(result)) {
                by <- c(by, "original_component")
            }
        }
        else {
            match <- "all"
        }
    }
    return(join(data, result, by=by, type="left", match=match))
}

get_summarize_columns <- function(item) {
    if (length(item$column) == 1 && length(item$summarize$operation) > 1) {
        return(paste(item$column, item$summarize$operation, sep="_"))
    }
    return(item$column)
}

get_features <- function(conn, features, exclude, items, data, colnames,
                         join_cols, details=F, required=c(), components=NULL) {
    if (length(features) == 1) {
        if (is.na(features)) {
            features <- unlist(sapply(items, function(item) { item$column }))
        }
        else {
            features <- unique(c(required,
                                 expand_feature_names(features, items)))
        }
    }
    if (isTRUE(details)) {
        details <- list()
    }
    selected_items <- c()
    expressions <- c()
    for (item in items) {
        if (include_feature(item, features, exclude, required)) {
            selected_items <- c(selected_items, list(item))
            columns <- get_summarize_columns(item)
            if (!is.null(item$result)) {
                result <- item$result
            }
            else if (!is.null(item$expression)) {
                expressions <- c(expressions, columns)
                if (isTRUE(item$precompute) || all(columns %in% required)) {
                    data <- get_expression(item, data, join_cols,
                                           components=components, merge=T)
                    colnames <- c(colnames, columns)
                }
                next
            }
            else if (is.null(item$query)) {
                stop(paste('No query or result available for', columns))
            }
            else {
                loginfo('Executing query for table %s', item$table)
                logdebug(item$query)
                time <- system.time(result <- dbGetQuery(conn, item$query))
                loginfo('Query for table %s took %f seconds', item$table,
                        time['elapsed'])
            }
            if (!is.null(components) && (!is.null(result$component) ||
                                         !is.null(item$summarize$component))) {
                component_field <- ifelse(!is.null(result$component),
                                          "component", item$summarize$component)
                result <- get_components(data, result, components,
                                         names(item$source)[1],
                                         component_field,
                                         summarize=item$summarize)
            }
            if (!is.null(item$summarize)) {
                summarize <- item$summarize
                group_names <- join_cols
                group_cols <- lapply(result[, group_names], factor)
                if (!is.null(components) && !is.null(result$component)) {
                    component_names <- get_component_names(components)
                    group_cols$component <- addNA(factor(result$component,
                                                         component_names))
                    group_names <- c(group_names, "component")
                    if (!is.null(result$original_component)) {
                        group_names <- c(group_names, "original_component")
                    }
                }
                groups <- split(result, as.list(group_cols), drop=T)
                if (nrow(result) == 0) {
                    result <- result[, group_names]
                    result[, columns] <- numeric()
                }
                else {
                    result <- do.call("rbind", lapply(groups, function(group) {
                        return(get_summarize_group(group, group_names, data,
                                                   columns, details, summarize))
                    }))
                }

                if (is.list(details)) {
                    filter <- parse(text=summarize$filter)
                    detailer <- function(group) {
                        group <- group[eval(filter, group), ]
                        group_details <- data.frame(group[1, group_names])
                        if (length(summarize$details) == 1) {
                            details <- list(list(group[, summarize$details]))
                        }
                        else {
                            details <- lapply(group[, summarize$details],
                                              function(detail) { list(detail) })
                        }
                        group_details[, summarize$details] <- details
                        return(group_details)
                    }
                    details[[item$column[1]]] <- lapply(groups, detailer)
                }
            }
            data <- merge_features(data, result, join_cols, components)
            if (isTRUE(item$carry)) {
                projects <- data[[join_cols[1]]]
                factors <- list(factor(data[[join_cols[1]]]))
                if ("component" %in% colnames(result)) {
                    factors$component <- addNA(factor(data$component))
                }
                groups <- split(data, as.list(factors), drop=T)
                group_locf <- function(group, column) {
                    group[, column] <- na.locf(group[, column], na.rm=F)
                    return(group)
                }
                data <- do.call("rbind",
                                lapply(groups, group_locf, columns))
            }
            if (!is.null(item$default)) {
                for (column in columns) {
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
            colnames <- c(colnames, columns)
        }
    }
    list(data=data, details=details, colnames=unique(colnames),
         join_cols=join_cols, items=selected_items, expressions=expressions)
}

get_summarize_group <- function(group, group_names, data, columns, details,
                                summarize) {
    group_result <- data.frame(group[1, group_names])
    n <- group_names[group_names != "original_component"]
    summarizer <- function(operation, field, reference, with_missing) {
        args <- list(group[, field])
        if (!is.na(reference) && is.list(details) &&
            !is.null(details[[reference]])) {
            name <- paste(group_result[n], collapse=".")
            args <- c(args, details[[reference]][[name]][[field]])
        }
        else if (!is.na(reference) && isTRUE(summarize$expression)) {
            # Find the current sprint data and use it
            f <- mapply(function(value, name) {
                            if (is.na(value)) {
                                return(is.na(data[[name]]))
                            }
                            return(data[[name]] == value)
                        },
                        group_result[, n], n)
            r <- rowSums(f, na.rm=T)
            ref <- data[r == length(n), reference]
            args <- c(args, list(reference=ref))
        }
        do.call(operation, c(args, list(na.rm=!with_missing)))
    }
    group_result[, columns] <- mapply(summarizer, summarize$operation,
                                      summarize$field, summarize$reference,
                                      summarize$with_missing)
    return(group_result)
}

get_expressions <- function(items, data, expressions, join_cols,
                            components=NULL) {
    for (item in items) {
        if (!is.null(item$expression) && all(item$column %in% expressions)) {
            data <- get_expression(item, data, join_cols, components, merge=F)
        }
    }
    return(data)
}

get_expression <- function(item, data, join_cols, components=NULL, merge=NA) {
    loginfo("Calculating expression %s", item$column)
    expression <- parse(text=item$expression)
    if (!is.null(item$window)) {
        group <- item$window$group
        if ("project_name" %in% colnames(data)) {
            group[group == "project_id"] <- "project_name"
        }
        else if (!("project_id" %in% colnames(data))) {
            group[group == "project_id"] <- join_cols[1]
        }
        if (length(group) == 1) {
            group_cols <- list(factor(data[, group]))
            names(group_cols) <- group
        }
        else {
            group_cols <- lapply(data[, group], factor)
        }
        groups <- split(data, as.list(group_cols), drop=T)
        all <- do.call("rbind", lapply(groups, function(group_data) {
            res <- eval(expression,
                        rbind(group_data[rep(1, item$window$dimension - 1), ],
                              group_data))
            result <- group_data[, join_cols]
            if (!is.null(components)) {
                result$component <- group_data$component
            }
            result[[item$column]] <- res
            return(result)
        }))
        if (identical(merge, F)) {
            data[[item$column]] <- all[[item$column]]
        }
        else {
            data[[item$column]] <- NULL
            data <- merge_features(data, all, join_cols, components)
        }
    }
    else {
        all <- eval(expression, data)
        data[, item$column] <- all
    }
    return(data)
}

get_expressions_metadata <- function(items, data) {
    assignments <- list()
    names(assignments) <- list()
    for (item in items) {
        if (!is.null(item$expression)) {
            attributes <- I(get_expression_attrs(item$expression, data))
            assignments[[item$column]] <- list(attributes=attributes,
                                               expression=item$expression)
        }
    }
    return(assignments)
}

get_expression_attrs <- function(expression, vars) {
    expr <- parse(text=expression)
    cond <- is.expression(expr)
    if (!cond) {
        return()
    }
    environment <- new.env()

    while (cond) {
        ref <- try(eval(expr, envir=environment), silent=T)
        if (cond <- (class(ref) == "try-error")) {
            if (length(grep("not found", ref[1])) > 0) {
                aux <- substr(ref, regexpr("object ", ref) + 8,
                              regexpr(" not found", ref) - 2)
                assign(as.character(aux), vars[[aux]], envir=environment)
            }
            else {
                logerror("expression %s could not be evaluated: %s",
                         expr, ref[1])
                return()
            }
        }
    }

    ls(envir=environment)
}

get_sprint_conditions <- function(latest_date='', core=F, sprint_days=NA,
                                  sprint_patch=NA, future=T) {
    conditions <- list()
    if (!is.character(latest_date) || latest_date != '') {
        conditions <- c(conditions,
                        '${t("sprint")}.start_date <= ${current_timestamp}')
    }
    if (core) {
        conditions <- c(conditions, 'COALESCE(is_support_team, false) = false',
                        'main_project IS NULL')
    }
    if (!is.na(sprint_days)) {
        conditions <- c(conditions,
                        paste('${s(sprint_close)} - ${t("sprint")}.start_date',
                              "> interval '${s(sprint_days)}' day"))
    }
    if (!is.na(sprint_patch)) {
        conditions <- c(conditions, ifelse(sprint_patch, '${s(sprint_patch)}',
                                           'NOT (${s(sprint_patch)})'))
    }
    if (!future) {
        conditions <- c(conditions, '${f("sprint_open", mask=1)} IS NOT NULL',
                        '${f("sprint_close", mask=1)} IS NOT NULL')
    }
    return(conditions)
}

get_components <- function(data, result, components, source_type, field,
                           summarize=NULL) {
    if (is.null(result$component)) {
        result$component <- rep(NA, nrow(result))
    }
    else if (field == "component") {
        result$original_component <- result$component
    }
    if (is.null(source_type)) {
        return(result)
    }

    for (component in components) {
        if (!(component$project %in% data$project_name)) {
            loginfo("No project %s for component %s found", component$project,
                    component$name)
            next
        }
        source_filter <- component[[source_type]]
        if (is.null(source_filter) && source_type == "jira") {
            source_filter <- list(include=component$name)
        }
        if (is.list(source_filter)) {
            project_id <- data[data$project_name == component$project,
                               "project_id"][[1]]
            project_condition <- result$project_id == project_id
            project_count <- length(which(project_condition))
            if (project_count == 0) {
                next
            }
            conditions <- project_condition
            if (!is.null(source_filter$include)) {
                conditions <- conditions &
                    result[, field] %in% source_filter$include
            }
            if (!is.null(source_filter$exclude)) {
                conditions <- conditions &
                    !(result[, field] %in% source_filter$exclude)
            }
            component_count <- length(which(conditions))
            loginfo('Setting %d results to component %s',
                    component_count, component$name)
            if (project_count == component_count && !isTRUE(component$remove)) {
                if (is.null(source_filter$exclude)) {
                    if (field == "component") {
                        original <- result[conditions, "component"]
                        result[conditions, "original_component"] <- original
                    }
                    result[conditions, "component"] <- NA
                }
                else if (field == "component") {
                    result[conditions, "original_component"] <- NA
                }
                rows <- result[conditions, ]
                rows$component <- component$name
                result <- rbind(result, rows)
            }
            else {
                if (field == "component") {
                    original <- result[conditions, "component"]
                    result[conditions, "original_component"] <- original
                }
                result[conditions, "component"] <- component$name
            }
            if (isTRUE(component$remove) && !is.null(summarize)) {
                key <- summarize$key
                remove <- project_condition & !conditions &
                    result[[key]] %in% result[conditions, key]
                loginfo('Removing %d from other components than %s',
                        length(which(remove)), component$name)
                result <- result[!remove, ]
            }
        }
    }
    return(result)
}

get_component_names <- function(components, source_type="name") {
    component_filter <- function(component) {
        source_filter <- component[[source_type]]
        if (is.null(source_filter) && source_type == "jira") {
            source_filter <- list(include=component$name)
        }
        if (!is.list(source_filter)) {
            return(source_filter)
        }
        if (!is.null(source_filter$include)) {
            return(source_filter$include)
        }
        return(source_filter$exclude)
    }
    return(unique(unlist(lapply(components, component_filter))))
}

get_story_features <- function(conn, features, exclude='^$',
                               latest_date=Sys.time(), changelog=T,
                               project_fields=list('project_id'),
                               project_meta=list(), project_names=NULL) {
    fields <- list(project_name='${t("project")}.name',
                   sprint_name='${t("sprint")}.name',
                   story_name='${t("issue")}.title',
                   latest='max_changelog.changelog_id IS NOT NULL')

    # Features from the table itself - TODO move to story_features.yml
    table <- list(overdue=paste('NOT ${s(issue_closed}) AND',
                                'CAST(${t("issue").duedate AS TIMESTAMP) <=',
                                '${current_timestamp}'),
                  num_attachments='${t("issue")}.attachments',
                  story_points='${s(story_points)}')

    join_cols <- c("project_id", "issue_id")
    if (changelog) {
        join_cols <- c(join_cols, "changelog_id")
    }

    query <- paste('SELECT ${f(join_cols, "issue")},',
                   paste(format_aliases(fields), collapse=", "),
                   'FROM gros.${t("issue")}
                    JOIN gros.${t("project")}
                    ON ${j(join_cols, "project", "issue", mask=1)}
                    LEFT JOIN gros.${t("sprint")}
                    ON ${j(join_cols, "sprint", "issue", mask=1)}
                    AND ${t("sprint")}.sprint_id = ${t("issue")}.sprint_id',
                    ifelse(changelog, 'LEFT', ''), 'JOIN (
                        SELECT ${f(join_cols, "issue", mask=1:2)},
                        MAX(changelog_id) AS changelog_id
                        FROM gros.${t("issue")}
                        ${g(join_cols, "issue", mask=1:2)}
                    ) AS max_changelog
                    ON ${j(join_cols, "issue", "max_changelog", mask=1:2)}
                    AND ${t("issue")}.changelog_id = max_changelog.changelog_id
                    WHERE ${t("issue")}.updated <= ${current_timestamp}
                    AND ${s(issue_story)}
                    ${s(project_condition)}')

    variables <- list(join_cols=list(default=join_cols))
    patterns <- load_definitions('sprint_definitions.yml', variables,
                                 current_time=latest_date)

    cond <- get_project_conditions(projects, join_cols, patterns,
                                   project_fields=project_fields,
                                   project_meta=project_meta,
                                   project_names=project_names)
    projects <- cond$projects
    patterns$project_condition <- cond$project_condition

    story_query <- load_query(list(query=query), patterns)
    logdebug(story_query$query)

    data <- dbGetQuery(conn, story_query$query)

    items <- load_queries('story_features.yml', NULL, patterns)
    result <- get_features(conn, features, exclude, items, data, c(), join_cols)
    result$projects <- projects

    return(result)
}

get_sprint_features <- function(conn, features, exclude, variables, latest_date,
                                core=F, sprint_days=NA, sprint_patch=NA,
                                future=T, combine=F, details=F, time=F,
                                teams=list()) {
    conditions <- get_sprint_conditions(latest_date, core, sprint_days,
                                        sprint_patch, future=future)
    if (length(conditions) != 0) {
        where_clause <- paste('WHERE', paste(conditions, collapse=' AND '))
        sprint_conditions <- paste('AND', paste(conditions, collapse=' AND '))
    }
    else {
        where_clause <- ''
        sprint_conditions <- ''
    }

    fields <- c('${f(join_cols, "sprint")}')
    if (config$db$primary_source == "tfs") {
        join_cols <- c("team_id", "sprint_id")
    } else {
        join_cols <- c("project_id", "sprint_id")
    }
    colnames <- c(join_cols, "sprint_num")
    if (time) {
        fields <- c(fields,
                    paste('CAST(${s(sprint_open)} AS DATE) -',
                          'date \'1970-01-01\' AS time'))
        colnames <- c(colnames, 'time')
    }
    order_by <- c('${f(join_cols, "sprint", mask=1)}', '${s(sprint_open)}',
               '${t("sprint")}.name')

    patterns <- load_definitions('sprint_definitions.yml',
                                 list(sprint_days=sprint_days,
                                      join_cols=join_cols),
                                 current_time=latest_date)

    query <- paste('SELECT', paste(fields, collapse=', '),
                   'FROM gros.${t("sprint")}
                    JOIN gros.${t("project")}
                    ON ${j(join_cols, "project", "sprint", mask=1)}',
                   where_clause,
                   'ORDER BY', paste(order_by, collapse=', '))
    sprint_query <- load_query(list(query=query), patterns)
    logdebug(sprint_query$query)

    sprint_data <- dbGetQuery(conn, sprint_query$query)

    items <- load_queries('sprint_features.yml', 'sprint_definitions.yml',
                          c(variables,
                            list(sprint_conditions=sprint_conditions,
                                 join_cols=join_cols)))

    result <- get_features(conn, features, exclude, items, sprint_data,
                           colnames, join_cols, details=details,
                           required=c("sprint_num"))
    expressions <- result$expressions
    if (!identical(combine, F)) {
        result <- get_combined_features(result$items, result$data,
                                        result$colnames, result$details,
                                        join_cols, combine=combine, teams=teams)
    }
    result$data <- get_expressions(result$items, result$data, expressions,
                                   join_cols)
    result$colnames <- c(result$colnames, expressions)
    result$patterns <- patterns
    return(result)
}

get_future_date <- function(group, last, future) {
    close <- ifelse(group[last, 'sprint_is_closed'], last, last - 1)
    start <- group[last, 'start_date'][[1]]
    length <- as.difftime(group[close, 'sprint_days'], units="days")
    downtime <- length + start - group[last - 1, 'close_date'][[1]]
    start_date <- seq(start + downtime, by=downtime, length.out=future)
    return(list(start_date=start_date,
                close_date=start_date + length,
                close=close))
}

make_future_sprints <- function(group, future, join_cols, colnames,
                                prediction_columns, late, last) {
    group[, names(prediction_columns)] <- NA
    if (future == 0 || last < 2 || !('sprint_days' %in% colnames) ||
        is.na(group[last, 'sprint_days'])) {
        return(group)
    }

    extra <- group[nrow(group), ]
    extra$old <- F
    extra$future <- T
    extra[[join_cols[2]]] <- 0
    extra$sprint_name <- 'Future Sprint'
    multi <- extra[rep(1, each=max(0, future - late)), ]
    multi$sprint_num <- seq(extra$sprint_num + 1, length.out=future - late)
    group <- rbind(group, multi)

    dates <- get_future_date(group, last, future)
    group[group$future, 'start_date'] <- dates$start_date
    group[group$future, 'close_date'] <- dates$close_date

    for (col in names(prediction_columns)) {
        prediction <- prediction_columns[[col]]
        if (prediction$ref %in% colnames &&
            group[dates$close, prediction$ref] > 0
        ) {
            steps <- seq(1, future) * group[dates$close, prediction$ref]
        }
        else {
            steps <- rep(0, future)
        }
        predict <- pmax(0, group[last, prediction$column] - steps)

        group[group$future, col] <- predict
    }
    down <- F
    if (length(prediction_columns) > 0) {
        # Remove future sprints where predicted values are all zero or negative
        down <- rowSums(group[, names(prediction_columns), drop=F] > 0) != 0
        down[group$future & !down][1] <- T
    }
    group <- group[!group$future | down, ]
    return(group)
}

update_non_recent_features <- function(group, future, limit, join_cols, items,
                                       colnames) {
    late <- length(which(group$future))
    last <- length(which(!group$future))
    first <- max(1, last - limit + 1)
    group[first:(last + late), 'old'] <- F

    prediction_columns <- list()
    real_columns <- list()
    for (item in items) {
        if (!is.null(item$prediction)) {
            group[group$future, item$column] <- 0
            real_columns[[item$column]] <- c()
        }
        for (prediction in item$prediction) {
            if (!is.null(prediction$reference)) {
                col <- paste(item$column, prediction$reference, sep='_')
                prediction_columns[[col]] <- list(column=item$column,
                                                  ref=prediction$reference)
                real_columns[[item$column]] <- c(real_columns[[item$column]],
                                                 prediction$reference)
            }
        }
    }

    group <- make_future_sprints(group, future, join_cols, colnames,
                                 prediction_columns, late, last)

    return(list(group=group, columns=real_columns,
                prediction_columns=prediction_columns, last=last))
}

validate_future <- function(project, res, future, join_cols, colnames, error) {
    group <- make_future_sprints(project, future, join_cols, colnames,
                                 res$prediction_columns, 0, nrow(project))
    error_columns <- list()
    for (col in names(res$columns)) {
        sprints <- group[group$future, res$columns[[col]]]
        bias <- colMeans(sprints - error[1:nrow(sprints), col])
        error_columns[[col]] <- bias / nrow(sprints)
        alt <- "two.sided"
        if (all(bias < 0, na.rm=T)) {
            alt <- "greater"
        }
        else if (all(bias > 0, na.rm=T)) {
            alt <- "less"
        }
        t_tests <- lapply(sprints,
                          function(x, y, alt) {
                              tryCatch(t.test(x, y, alt)$p.value,
                                       error=function(cond) { NA })
                          },
                          error[1:nrow(sprints), col], alt)
        probabilities <- as.data.frame(t_tests)
        colnames(probabilities) <- res$columns[[col]]
        error_columns[[paste(col, 'probability', sep='_')]] <- probabilities
    }
    return(error_columns)
}

simulate_monte_carlo <- function(group, future, items, columns, last=NA,
                                 name='density', target=0, count=10000) {
    # Calculate the cumulative density at each sprint
    res <- list()
    if (is.na(last)) {
        last <- length(which(!group$future))
    }
    for (item in items) {
        if (!is.null(item$prediction)) {
            column <- paste(item$column, name, sep='_')
            res[[column]] <- list()
            for (prediction in item$prediction) {
                if (!is.null(prediction$monte_carlo)) {
                    counts <- rep(NA, count)
                    samples <- rep(0, future * count)
                    for (factor in prediction$monte_carlo$factors) {
                        weights <- dgamma(last:1, 1, rate=0.5)
                        multipliers <- group[!group$future &
                                             !is.na(group[[factor$multiplier]]),
                                             factor$multiplier]
                        samples <- samples +
                            factor$scalar * multipliers[length(multipliers)] *
                            sample(group[1:last, factor$column],
                                   future * count, replace=T, prob=weights)
                    }
                    for (i in 1:count) {
                        end <- group[last, item$column] +
                            cumsum(samples[(future * (i-1) + 1):(future * i)])
                        if (any(end < target, na.rm=T)) {
                            counts[i] <- which(end <= target)[1]
                        }
                    }
                    if (all(is.na(counts))) {
                        cdf <- list()
                    }
                    else {
                        P <- ecdf(counts)
                        cdf <- P(seq(future))
                    }
                    res[[column]][[prediction$monte_carlo$name]] <- cdf
                }
            }
            if (length(res[[column]]) == 0) {
                res[[column]] <- NULL
            }
        }
    }
    return(res)
}

calculate_feature_scores <- function(data, column, join_cols) {
    meta_columns <- c(join_cols, 'sprint_name', 'board_id', 'start_date',
                      'close_date', 'future', 'old')
    selectors <- data[!data$future, !(names(data) %in% meta_columns)]
    selectors$target <- selectors[[column]]
    selectors[[column]] <- NULL
    if (ncol(selectors) > 1) {
        estimators <- c("RReliefFequalK", "ReliefFexpRank", "RReliefFwithMSE",
                        "MSEofMean")
        for (estimator in estimators) {
            loginfo("%s selection for estimating %s", estimator, column)
            attr <- attrEval(target ~ ., data=selectors, estimator=estimator)
            print(attr[order(attr, decreasing=T)])
        }
    }
}

get_project_conditions <- function(projects, join_cols, patterns, date=NA,
                                   project_fields=list('project_id'),
                                   project_meta=list(), project_names=NULL) {
    if (!is.na(date)) {
        project_meta$recent <- date
    }
    else {
        project_meta$recent <- T
    }
    projects <- get_projects_meta(conn, fields=project_fields,
                                  metadata=c(project_meta, list(main=T)),
                                  join_cols=join_cols, patterns=patterns)
    projects$project_ids <- lapply(projects$project_id,
                                   function(project_id) { list(project_id) })
    projects$project_names <- projects$name
    projects <- projects[projects$main, ]
    if (length(project_names) > 0) {
        projects <- projects[projects$name %in% project_names, ]
    }

    project_condition <- paste('AND ${f(join_cols, "project", mask=1)} IN (',
                               paste(projects$project_id, collapse=','), ')')
    patterns$project_condition <- project_condition
    return(list(projects=projects,
                project_condition=project_condition))
}

get_recent_sprint_features <- function(conn, features, exclude='^$', date=NA,
                                       limit=5, closed=T, sprint_conditions=c(),
                                       project_fields=list('project_id'),
                                       project_meta=list(), old=F, future=0,
                                       details=F, combine=F, teams=list(),
                                       project_names=NULL, components=NULL,
                                       prediction=list(), scores=F,
                                       latest_date=Sys.time(),
                                       variables=list()) {
    fields <- list(project_name='${t("project")}.name',
                   sprint_name='${t("sprint")}.name',
                   start_date='${s(sprint_open)}',
                   close_date='COALESCE(${s(sprint_close)}, ${s(sprint_open)})',
                   old='${old}',
                   future='${future}')

    names(project_fields) <- project_fields
    primary_source <- config$db$primary_source
    if (primary_source == "tfs") {
        join_cols <- c("team_id", "sprint_id")
        project_fields$project_id <- "team_id"
        project_fields$quality_display_name <- NULL
    } else {
        fields <- c(fields,
                    list(quality_display_name='project.quality_display_name',
                         quality_name='project.quality_name'))
        if (primary_source == "jira_version") {
            join_cols <- c("project_id", "fixversion")
        }
        else {
            join_cols <- c("project_id", "sprint_id")
            fields$board_id <- 'sprint.board_id'
        }
    }
    order_by <- c('${f(join_cols, "sprint", mask=1)}',
                  '${s(sprint_open)} DESC',
                  '${t("sprint")}.name DESC')

    if (future == 0) {
        sprint_conditions <- c(sprint_conditions,
                               '${s(sprint_open)} < ${current_timestamp}')
    }
    if (closed) {
        sprint_conditions <- c(sprint_conditions,
                               '${s(sprint_close)} < ${current_timestamp}')
    }
    colnames <- c(join_cols, names(fields)[names(fields) != ""])
    sprint_conditions <- paste(sprint_conditions, collapse=' AND ')

    variables <- c(variables,
                   list(sprint_conditions=sprint_conditions,
                        join_cols=list(default=join_cols),
                        jira_join='',
                        component_join=''))
    query_join <- ''
    if (identical(combine, F)) {
        components <- NULL
    }
    if (!is.null(components)) {
        component_join <- 'LEFT JOIN gros.${t("component")}
                           ON ${j(join_cols, "project", "component", mask=1)}
                           AND ${t("component")}.name IN (${components})'
        if (primary_source == "jira_component_version") {
            jira_join <- 'LEFT JOIN gros.fixversion
                          ON ${t("issue")}.fixversion = fixversion.id
                          AND fixversion.name IN (${components})'
            component_join <- paste(component_join,
                                    'AND ${t("component")}.start_date <=
                                     CAST(${t("sprint")}.start_date AS DATE)')
        }
        else {
            jira_join <- 'LEFT JOIN gros.${t("issue_component")}
                          ON ${j("issue_id", "issue", "issue_component")}
                          AND ${t("issue_component")}.end_date IS NULL
                          ${s(component_join, project="issue")} AND
                          ${j("component_id", "issue_component", "component")}'
        }

        component_names <- get_component_names(components, "jira")

        variables$issue_join <- jira_join
        variables$component_join <- component_join
        component <- get_primary_tables()$component
        component_join_cols <- list(component=paste(component, 'name', sep="."))
        variables$join_cols$jira <- component_join_cols
        variables$join_cols[[primary_source]] <- component_join_cols
        variables$components <- paste(dbQuoteString(conn, component_names),
                                      collapse=", ")
        colnames <- c(colnames, "component")
    }
    query <- paste('SELECT ${f(join_cols, "sprint")}, ',
                   paste(format_aliases(fields), collapse=", "),
                   'FROM gros.${t("sprint")}
                    JOIN gros.${t("project")}
                    ON ${j(join_cols, "project", "sprint", mask=1)}
                    ${s(component_join)}
                    WHERE ${s(sprint_conditions)}
                    ${s(project_condition)}
                    ORDER BY', paste(order_by, collapse=", "), '${limit}')

    patterns <- load_definitions('sprint_definitions.yml', variables,
                                 current_time=latest_date)

    cond <- get_project_conditions(projects, join_cols, patterns, date=date,
                                   project_fields=project_fields,
                                   project_meta=project_meta,
                                   project_names=project_names)
    projects <- cond$projects
    patterns$project_condition <- cond$project_condition
    if (old) {
        # Old value is calculated by combined data later on
        item <- load_query(list(query=query),
                           c(patterns, list(old='TRUE',
                                            future='FALSE',
                                            limit='',
                                            source=primary_source)))
        logdebug(item$query)
        sprint_data <- dbGetQuery(conn, item$query)
    }
    else {
        projects <- projects[projects$recent, ]
        sprint_data <- data.frame()
        for (project in projects$project_id) {
            condition <- paste('AND ${f(join_cols, "sprint", mask=1)} =',
                               project)

            item <- load_query(list(query=query),
                               modifyList(patterns,
                                          list(project_condition=condition,
                                               old='FALSE',
                                               future='FALSE',
                                               limit=paste('LIMIT', limit),
                                               source=primary_source)))
            sprint_data <- rbind(sprint_data, dbGetQuery(conn, item$query))
        }
    }
    if (!is.null(components)) {
        # Ensure projects that are missing/combining components are still
        # distributed into components according to include/exclude conditions
        sprint_data <- get_components(sprint_data, sprint_data, components,
                                      "jira", "component")
    }
    sprint_data$start_date <- as.POSIXct(sprint_data$start_date)
    sprint_data$close_date <- as.POSIXct(sprint_data$close_date)
    sprint_data$future <-
        as.Date(sprint_data$start_date) >= as.Date(latest_date)
    sprint_data <- arrange(sprint_data, sprint_data$project_name,
                           sprint_data$start_date, sprint_data$sprint_name)

    data <- yaml.load_file('sprint_features.yml')
    items <- list()
    required <- c("sprint_num", "sprint_days", "sprint_is_closed")
    for (item in data$files) {
        if (include_feature(item, features, exclude, required)) {
            items <- c(items, list(load_query(item, patterns, data$path)))
        }
    }

    result <- get_features(conn, features, exclude, items, sprint_data,
                           colnames, join_cols, details=details,
                           required=required, components=components)
    expressions <- result$expressions

    result$projects <- projects
    if (prediction$data != '' && !identical(prediction$combine, F)) {
        result <- get_prediction_feature(prediction, result, join_cols)
    }
    if (!identical(combine, F)) {
        result <- get_combined_features(result$items, result$data,
                                        result$colnames, result$details,
                                        join_cols, combine=combine, teams=teams,
                                        limit=limit, date=project_meta$recent,
                                        main=T, projects=result$projects,
                                        components=components)
    }
    result$data <- get_expressions(result$items, result$data, expressions,
                                   join_cols, components)
    result$colnames <- c(result$colnames, expressions)
    if (prediction$data != '' && identical(prediction$combine, F)) {
        result <- get_prediction_feature(prediction, result, join_cols)
    }

    if (old || future > 0) {
        project_data <- split(result$data, result$data[, 'project_name'])
        result$data <- data.frame()
        result$errors <- list()
        for (project in project_data) {
            res <- update_non_recent_features(project, future, limit, join_cols,
                                              result$items, result$colnames)
            if (future > 0 && nrow(res$group) > 1) {
                project_name <- project[1, 'project_name']
                num_sprints <- as.integer(nrow(project) / 3) + 1
                second_sprints <- (num_sprints - 1) * 2 + 1
                first <- validate_future(project[1:num_sprints, ],
                                         res, num_sprints, join_cols,
                                         result$colnames,
                                         project[-1:-num_sprints, ])
                second <- validate_future(project[num_sprints:second_sprints, ],
                                          res, num_sprints, join_cols,
                                          result$colnames,
                                          project[-1:-second_sprints, ])
                errors <- mapply(function(one, two) {
                                     as.list(as.data.frame(rbind(one, two)))
                                 },
                                 first, second, SIMPLIFY=F)

                predictions <- res$group[nrow(res$group),
                                         names(res$prediction_columns)]
                initial <- res$group[res$last,
                                     unlist(lapply(res$prediction_columns,
                                                   function(p) { p$column }))]
                more <- ifelse(length(predictions) > 0 && length(initial) > 0 &&
                               any(predictions != initial & predictions > 0),
                               future * 2, future)

                dates <- get_future_date(res$group, res$last, more)
                errors <- c(errors, simulate_monte_carlo(res$group, more,
                                                         result$items,
                                                         res$columns,
                                                         dates$close))
                errors$date <- dates$start_date

                result$errors[[project_name]] <- as.list(errors)
            }
            result$data <- rbind(result$data, res$group)
        }
    }

    if (scores) {
        for (item in result$items) {
            if (!is.null(item$prediction)) {
                calculate_feature_scores(result$data, item$column, join_cols)
            }
        }
    }

    for (item in result$items) {
        if (!is.null(item$summarize) && length(item$summarize$operation) > 1) {
            loginfo("Wrapping column %s", item$column)
            result <- wrap_feature(item, item$summarize$operation, result)
        }
        if (!is.null(item$prediction) &&
            !is.null(item$prediction[[1]]$reference)) {
            loginfo("Wrapping predictions for %s", item$column)
            predictions <- unlist(lapply(item$prediction,
                                         function(prediction) {
                                             return(prediction$reference)
                                         }))
            result <- wrap_feature(item, predictions, result,
                                   result$data$future)
        }
    }
    result$project_fields <- project_fields
    result$patterns <- patterns
    return(result)
}

wrap_feature <- function(item, operations, result, filter=T) {
    cols <- paste(item$column, operations, sep="_")
    if (!(item$column %in% result$colnames)) {
        result$data[, item$column] <- rep(0, nrow(result$data))
    }
    wrap <- apply(result$data[filter, cols], 1,
                  function(...) {
                      args <- as.list(...)
                      names(args) <- operations
                      return(I(args))
                  })
    result$data[filter, ][[item$column]] <- wrap
    result$data[, cols] <- NULL
    result$colnames <- c(result$colnames[!(result$colnames %in% cols)],
                         item$column)
    return(result)
}

get_prediction_feature <- function(prediction, result, join_cols) {
    loginfo('Collecting predictions from %s', prediction$data)
    data <- fromJSON(url(prediction$data))
    predictions <- do.call("rbind",
                           mapply(function(labels, projects, sprints) {
                                      data.frame(projects, sprints, labels,
                                                 fix.empty.names=F)
                           },
                           data$labels, data$projects, data$sprints,
                           SIMPLIFY=F, USE.NAMES=F))
    colnames(predictions) <- c(join_cols, "prediction")
    result$data <- join(result$data, predictions,
                        by=join_cols, type="left",
                        match="first")
    item <- list(column="prediction",
                 combine=prediction$combine,
                 values=list(type="fraction",
                             denominator=4),
                 descriptions=list(nl="Voorspelde storypoints",
                                   en="Predicted story points"),
                 long_descriptions=list(nl=paste("Voorspelling van het aantal",
                                                 "storypoints dat in de sprint",
                                                 "zou worden gerealiseerd op",
                                                 "basis van historische data"),
                                        en=paste("Prediction of the number of",
                                                 "story points that could be",
                                                 "realized during the sprint",
                                                 "based on historical data")),
                 source=list(prediction=prediction$source),
                 measurement=list(unit='point'))
    result$items <- c(result$items, list(item))
    result$colnames <- c(result$colnames, "prediction")
    return(result)
}

get_project_features <- function(conn, features, exclude, variables, core=F,
                                 metadata=list(), project_fields=list()) {
    names(project_fields) <- project_fields
    if (config$db$primary_source == "tfs") {
        join_cols <- c("team_id")
        project_fields$project_id <- NULL
        project_fields$team_id <- "team_id"
        project_fields$quality_display_name <- NULL
    }
    else {
        join_cols <- c("project_id")
    }
    patterns <- load_definitions('sprint_definitions.yml',
                                 c(variables, list(join_cols=join_cols)))
    if (isTRUE(core)) {
        metadata$core <- T
    }
    data <- get_projects_meta(conn, project_fields, metadata, join_cols,
                              patterns, by='name')
    if (isTRUE(core)) {
        data <- data[data$core, ]
    }
    projects <- data

    items <- load_queries('project_features.yml', NULL, patterns)
    result <- get_features(conn, features, exclude, items, data, c(), join_cols)
    result$projects <- projects
    result$project_fields <- project_fields
    result$patterns <- patterns
    return(result)
}

write_feature_metadata <- function(projects, specifications, output_directory,
                                   features=c(), items=c(),
                                   locales=c('descriptions',
                                             'long_descriptions', 'units',
                                             'short_units', 'tags',
                                             'predictor'),
                                   metadata=c('values', 'measurement',
                                              'preferred', 'prediction')) {
    if (length(items) == 0) {
        items <- specifications$files
    }
    for (locale in locales) {
        write(toJSON(get_feature_locales(items, locale)),
              file=paste(output_directory, paste(locale, "json", sep="."),
                         sep="/"))
    }
    write(toJSON(projects, auto_unbox=T),
          file=paste(output_directory, "projects.json", sep="/"))

    source_types <- yaml.load_file("source_types.yml")
    sources <- get_locales(source_types)
    if (length(features) > 0) {
        cats <- specifications$categories
        meta <- rep(list(NULL), length(metadata))
        names(meta) <- metadata
        sources$feature <- list()

        for (item in items) {
            feature <- item$column[item$column %in% features]
            if (length(feature) > 0) {
                cat <- ifelse("category" %in% names(item), item$category,
                              "other")
                cats[[cat]]$items <- c(cats[[cat]]$items, feature)

                metas <- lapply(item[metadata[metadata %in% names(item)]],
                                function(fields) {
                                    if (length(item$column) == 1) {
                                        field <- list(fields)
                                        names(field) <- item$column
                                        return(field)
                                    }
                                    return(mapply(function(column, value) {
                                                      return(value)
                                                  },
                                                  item$column, metas,
                                                  SIMPLIFY=F))
                                })
                meta <- modifyList(meta, metas)
                for (source_name in names(item$source)) {
                    if (source_name %in% names(source_types)) {
                        current <- sources$feature[[source_name]]
                        sources$feature[[source_name]] <- c(current, feature)
                    }
                }
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
        write(toJSON(meta, auto_unbox=T),
              file=paste(output_directory, "metadata.json", sep="/"))
    }
    write(toJSON(sources),
          file=paste(output_directory, "sources.json", sep="/"))
}
