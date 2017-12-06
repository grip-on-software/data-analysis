library(MonetDB.R)
library(DBI)
library(digest)
library(ggplot2)
library(AnomalyDetection)
library(xts)

source('include/database.r')
conn <- connect()

projects <- dbGetQuery(conn, 'SELECT project_id, name FROM gros.project;')

apply(projects, 1, function(row) {
	print(row[1])
	statuses <- dbGetQuery(conn, paste('SELECT name, checked_date, value FROM gros.bigboat_status
WHERE project_id =', row[1], " AND name NOT LIKE 'Agent %' AND name <> 'System Uptime' AND value IS NOT NULL ORDER BY name, checked_date;"))

	status_data <- split(statuses, statuses['name'])
	lapply(names(status_data), function(name) {
		data <- status_data[[name]]
		if (nrow(data) == 0) {
			print('No data')
			return()
		}

		# 1 measurement per 15 minutes (4 per hour), the whole week.
		if (nrow(data) < 4*24*7*2) {
			print('Too little data')
			return()
		}
		res <- AnomalyDetectionVec(data$value, max_anoms=0.02, direction='pos', plot=TRUE, period=4*24*7)
		print(str(res))
		if (!is.null(res$plot)) {
			print(res$plot + ggtitle(paste("Anomalies for project", row[2], "for BigBoat status", name)))
			Sys.sleep(1)
		}

	})

})
