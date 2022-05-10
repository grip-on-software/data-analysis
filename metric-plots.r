# Script to plot metric values.
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

library(MonetDB.R)
library(DBI)
library(digest)
library(AnomalyDetection)
library(xts)

source('include/database.r')
conn <- connect()

metrics <- dbGetQuery(conn, 'SELECT metric_id, name FROM gros.metric;')
summary(metrics)

apply(metrics, 1, function(row) {
    data <- dbGetQuery(conn, paste('SELECT date, value FROM gros.metric_value
                                    WHERE metric_id =', row[1],
                                    'ORDER BY date'))

    nrow(data)
    summary(data)

    data[[1]] <- as.POSIXlt(data[[1]])

    nrow(data)
    summary(data)

    a <- min(data[[1]])
    b <- max(data[[1]])

    difference <- as.numeric(median(diff(data[[1]])))
    points <- a + seq(0, (60*60/difference)*24*as.numeric(b-a))*difference
    sample_points <- xts(rep(NA, length(points)), order.by=points)

    xdata <- xts(data[[2]], order.by=data[[1]])
    samples <- merge(sample_points, na.locf(merge(sample_points, xdata)[, 2],
                                            gap=1), join="inner")[, 2]

    plot(samples, main=row[2])
})
