#library(ggplot2)
#library(reshape2)

source('include/database.r')

conn <- connect()

authors <- dbGetQuery(conn, '
SELECT commitdata.project_id AS project_id, jiradata.jira_developer_id AS developer, MIN(commitdata.min_date) AS commit_min, MAX(commitdata.max_date) AS commit_max, MIN(jiradata.min_date) AS jira_min, MAX(jiradata.max_date) AS jira_max
FROM
(SELECT project_id, developer_id, MIN(commit_date) AS min_date, MAX(commit_date) AS max_date
FROM gros.commits
GROUP BY project_id, developer_id) AS commitdata
LEFT JOIN
((SELECT project_id, updated_by AS jira_developer_id, MIN(updated) AS min_date, MAX(updated) AS max_date
FROM gros.issue
GROUP BY project_id, jira_developer_id) AS jiradata
JOIN
(gros.git_developer JOIN gros.developer ON git_developer.jira_dev_id = developer.id)
ON jiradata.jira_developer_id = developer.name)
ON commitdata.project_id = jiradata.project_id
AND commitdata.developer_id = git_developer.alias_id
GROUP BY commitdata.project_id, jiradata.jira_developer_id
ORDER BY commitdata.project_id, jiradata.jira_developer_id
')

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')
sprints <- dbGetQuery(conn, 'SELECT sprint.project_id, sprint.sprint_id, sprint.start_date, sprint.end_date FROM gros.sprint ORDER BY sprint.start_date')

total_data <- data.frame()
for (project in projects$project_id) {
	print(paste("PROJECT", project))
	project_name <- projects[projects$project_id == project,'name']
	print(project_name)
	project_sprints <- sprints[sprints$project_id == project,]

	sprint_count <- dim(project_sprints)[1]
	sprint_authors <- data.frame(project=character(sprint_count), time=character(sprint_count), count=numeric(sprint_count), stringsAsFactors=FALSE)
	for (sprint in 1:sprint_count) {
		start_date <- project_sprints[sprint,'start_date']
		end_date <- project_sprints[sprint,'end_date']

		sprint_authors$project[sprint] = project_name
		sprint_authors$time[sprint] = as.POSIXct(end_date)
		sprint_authors$count[sprint] = nrow(authors[authors$project_id == project & ((authors$commit_min < end_date & authors$commit_max > start_date) | (authors$jira_min < end_date & authors$jira_max > start_date)),])
	}
	print(sprint_authors)
}
