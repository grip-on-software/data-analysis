# Functions for obtaining metric metadata.
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

library(jsonlite)
source('include/database.r')

get_metric_targets <- function(conn, project_id, items) {
    base_names <- unlist(lapply(items, function(item) { item$metric }))
    if (length(base_names) == 0) {
        return(data.frame())
    }
    conditions <- list(base_name=paste('base_name IN (',
                                       paste(dbQuoteString(conn, base_names),
                                             collapse=','), ')'))
    if (length(project_id) == 1 && is.na(project_id)) {
        query <- paste('SELECT base_name, commit_date, direction,
                        perfect, target, low_target
                        FROM gros.metric_default
                        WHERE', paste(conditions, collapse=' AND '),
                       'ORDER BY base_name, commit_date')

    } else {
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
    names(feature_targets) <- list()
    for (item in items) {
        if (!is.null(item$metric)) {
            feature <- targets[targets$base_name == item$metric[1], ]
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
