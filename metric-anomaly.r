library(MonetDB.R)
library(DBI)
library(digest)
library(AnomalyDetection)
library(xts)

source('database.r')
conn <- connect()

metrics <- dbGetQuery(conn, 'SELECT metric_id, name FROM gros.metric;')
summary(metrics)

apply(metrics, 1, function(row) {
	data <- dbGetQuery(conn, paste('SELECT date, value FROM gros.metric_value
WHERE metric_id =', row[1], ' ORDER BY date;'))

	nrow(data)
	summary(data)

	data[[1]] = as.POSIXlt(data[[1]])

	nrow(data)
	summary(data)

	a <- min(data[[1]])
	b <- max(data[[1]])

	stamps <- do.call(rbind.data.frame, apply(data, 1, function(row) data.frame(date=as.POSIXct(row[1]), value=row[2])))

	nrow(stamps)
	ncol(stamps)
	summary(stamps)

	# 1 measurement per 15 minutes (4 per hour) during a weekday from 8 to 18 hours during a 2-week long sprint
	res <- AnomalyDetectionVec(data[,2], max_anoms=0.02, direction='pos', longterm=TRUE, plot=TRUE, period=4*10*5*2)
	res$plot

})
