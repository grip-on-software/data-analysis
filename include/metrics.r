# Functions for obtaining metric metadata

library(jsonlite)
source('include/database.r')

get_metric_targets <- function(conn, project_id, items) {
    base_names <- unlist(lapply(items, function(item) { item$metric }))
    conditions <- list(base_name=paste('base_name IN (',
                                       paste(dbQuoteString(conn, base_names),
                                             collapse=','), ')'))
    if (length(project_id) == 1 && is.na(project_id)) {
        query <- paste('SELECT base_name, commit_date, direction,
                        perfect, target, low_target
                        FROM gros.metric_default
                        WHERE', paste(conditions, collapse=' AND '),
                       'ORDER BY base_name, commit_date')

    }
    else {
        conditions$project_id <- paste('metric_target.project_id IN (',
                                       paste(project_id, collapse=','), ')')
        query <- paste('SELECT metric_target.project_id, metric.base_name,
                        metric.domain_name, metric_version.commit_date,
                        target, low_target
                        FROM gros.metric_target
                        JOIN gros.metric_version
                        ON metric_target.version_id = metric_version.version_id
                        JOIN gros.metric
                        ON metric_target.metric_id = metric.metric_id
                        WHERE', paste(conditions, collapse=' AND '),
                       'ORDER BY metric_target.project_id, metric.base_name,
                        metric.domain_name, metric_version.commit_date')
    }

    dbGetQuery(conn, query)
}

write_metric_targets <- function(targets, output_directory, items) {
    feature_targets <- list()
    for (item in items) {
        if (!is.null(item$metric)) {
            feature <- targets[targets$base_name == item$metric, ]
            feature$project_id <- NULL
            feature$base_name <- NULL
            feature$date <- as.POSIXct(feature$commit_date)
            feature$commit_date <- NULL
            if ("domain_name" %in% colnames(feature)) {
                feature <- lapply(split(feature, feature$domain_name),
                                  function(domain) {
                                      domain$domain_name <- NULL
                                      return(domain)
                                  })
            }
            feature_targets[[item$column]] <- feature
        }
    }
    write(toJSON(feature_targets),
          file=paste(output_directory, "metric_targets.json", sep="/"))
}
