library(jsonlite)
source('database.r')

conn <- connect()

queries <- c(
			 'SELECT project."name" AS project_name, sprint."name" AS sprint_name, sprint.start_date AS date, \'sprint_start\' AS type
			 FROM gros.sprint
			 LEFT JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 'SELECT project."name" AS project_name, sprint."name" AS sprint_name, sprint.end_date AS date, \'sprint_end\' AS type
			 FROM gros.sprint
			 LEFT JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 'SELECT project."name" AS project_name, sprint."name" AS sprint_name, issue.updated AS date, \'rank_change\' AS type
			 FROM gros.issue
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 LEFT JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.rank_change IS NOT NULL AND updated < sprint.end_date
			 ORDER BY project.project_id, sprint.start_date, issue.updated;'
			 )

data <- data.frame()

for (query in queries) {
	data <- rbind(data, dbGetQuery(conn, query))
}

data[['date']] = as.POSIXct(data[['date']])

projects <- dbGetQuery(conn, 'SELECT project."name" FROM gros.project ORDER BY project.project_id')

project_data = list()
for (project in projects[['name']]) {
	project_data[[project]] <- data[data$project_name == project,]
}

write(toJSON(project_data), file="data.json")
