library(jsonlite)
library(plyr)
source('database.r')

conn <- connect()

queries <- c(
			 # sprint start
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS date, sprint.end_date AS end_date, \'sprint_start\' AS type
			 FROM gros.sprint
			 LEFT JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 # sprint end
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.end_date AS date, \'sprint_end\' AS type
			 FROM gros.sprint
			 LEFT JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 # rank change
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'rank_change\' AS type
			 FROM gros.issue
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 LEFT JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.rank_change IS NOT NULL AND updated < sprint.end_date
			 ORDER BY project.project_id, sprint.start_date, issue.updated;'
			 ,
			 # storypoint change
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'storypoint_change\' AS type
			 FROM gros.issue
			 LEFT JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 LEFT JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.story_points <> prev_issue.story_points AND issue.updated < sprint.end_date
			 ORDER BY project.project_id, sprint.start_date, issue.updated;'
			 ,
			 # Metrics problem
			 'SELECT DISTINCT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, since_date AS date, \'red_metric\' AS type
			 FROM gros.metric_value
			 LEFT JOIN gros.project ON metric_value.project_id = project.project_id
			 LEFT OUTER JOIN gros.sprint ON metric_value.project_id = sprint.project_id AND metric_value.since_date BETWEEN sprint.start_date AND sprint.end_date
			 WHERE category = \'red\' AND EXTRACT(day FROM date - since_date) > 7'
			 ,
			 # Impediment
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'impediment\' AS type
			 FROM gros.issue
			 LEFT JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 LEFT JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.impediment = TRUE AND prev_issue.impediment = FALSE'
			 )

data <- data.frame()

for (query in queries) {
	data <- rbind.fill(data, dbGetQuery(conn, query))
}

data$date = format(as.POSIXct(data$date), format="%Y-%m-%dT%H:%M:%S")
data$end_date = format(as.POSIXct(data$end_date), format="%Y-%m-%dT%H:%M:%S")

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

# Commits
commits <- dbGetQuery(conn,
					  'SELECT project."name" AS project_name, sprint."name" AS sprint_name, commits.commit_date AS date, \'commits\' AS type
					   FROM gros.commits
					   LEFT OUTER JOIN gros.sprint ON commits.commit_date BETWEEN sprint.start_date AND sprint.end_date
					   LEFT JOIN gros.project ON commits.project_id = project.project_id')

project_data = list()
commit_data = list()
apply(projects, 1, function(project) {
	project_name <- project['name']
	project_id <- project['project_id']
	project_data[[project_name]] <- data[data$project_name == project_name,]

	path <- paste("output", project_name, sep="/")
	dir.create(path)

	sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id, sprint."name" FROM gros.sprint WHERE sprint.project_id =', project_id))
	apply(sprints, 1, function(sprint) {
		sprint_name <- sprint['name']
		sprint_id <- sprint['sprint_id']
		sprint_commits <- commits[commits$project_name == project_name & commits$sprint_name == sprint_name,]

		filename = paste(path, paste("commits", sprint_id, "json", sep="."), sep="/")
		write(toJSON(sprint_commits), file=filename)
	})
})

total_data = list(min_date=min(data$date),
				  max_date=max(data$date),
				  projects=project_data)

write(toJSON(total_data), file="output/data.json")
