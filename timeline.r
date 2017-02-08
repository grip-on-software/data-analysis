library(jsonlite)
library(plyr)
source('database.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

conn <- connect()

queries <- c(
			 # sprint start
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS date, sprint.end_date AS end_date, \'sprint_start\' AS type
			 FROM gros.sprint
			 JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 # sprint end
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.end_date AS date, \'sprint_end\' AS type
			 FROM gros.sprint
			 JOIN gros.project ON sprint.project_id = project.project_id'
			 ,
			 # rank change
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'rank_change\' AS type
			 FROM gros.issue
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.rank_change IS NOT NULL AND updated < sprint.end_date
			 ORDER BY project.project_id, sprint.start_date, issue.updated;'
			 ,
			 # storypoint change
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'storypoint_change\' AS type
			 FROM gros.issue
			 JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.story_points <> prev_issue.story_points AND issue.updated < sprint.end_date
			 ORDER BY project.project_id, sprint.start_date, issue.updated;'
			 ,
			 # Metrics problem
			 'SELECT DISTINCT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, since_date AS date, \'red_metric\' AS type
			 FROM gros.metric_value
			 JOIN gros.project ON metric_value.project_id = project.project_id
			 LEFT OUTER JOIN gros.sprint ON metric_value.project_id = sprint.project_id AND metric_value.since_date BETWEEN sprint.start_date AND sprint.end_date
			 WHERE category = \'red\' AND EXTRACT(day FROM date - since_date) > 7'
			 ,
			 # Impediment
			 'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'impediment\' AS type
			 FROM gros.issue
			 JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
			 LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
			 JOIN gros.project ON issue.project_id = project.project_id
			 WHERE issue.impediment = TRUE AND prev_issue.impediment = FALSE'
			 )

data <- data.frame()

for (query in queries) {
	data <- rbind.fill(data, dbGetQuery(conn, query))
}

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

# Commits
commits <- dbGetQuery(conn,
					  'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, commits.commit_date AS date, \'commits\' AS type
					   FROM gros.commits
					   JOIN gros.sprint ON commits.commit_date BETWEEN sprint.start_date AND sprint.end_date
					   JOIN gros.project ON commits.project_id = project.project_id')

data$date = dateFormat(data$date)
data$end_date = dateFormat(data$end_date)
commits$date = dateFormat(commits$date)

project_data = lapply(as.list(1:dim(projects)[1]), function(project) {
	project_name <- projects[project,'name']
	project_id <- projects[project,'project_id']

	path <- paste("output", project_name, sep="/")
	if (!dir.exists(path)) {
		dir.create(path)
	}

	sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id FROM gros.sprint WHERE sprint.project_id =', project_id))
	for (sprint_id in sprints$sprint_id) {
		sprint_commits <- commits[commits$project_name == project_name & commits$sprint_id == sprint_id,]

		filename = paste(path, paste("commits", sprint_id, "json", sep="."), sep="/")
		write(toJSON(sprint_commits), file=filename)
	}

	data[data$project_name == project_name,]
})
names(project_data) <- projects$name

total_data = list(min_date=min(data$date),
				  max_date=max(data$date),
				  update_date=dateFormat(Sys.time()),
				  projects=project_data)

write(toJSON(total_data), file="output/data.json")
