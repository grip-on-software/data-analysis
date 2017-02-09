library(jsonlite)
library(plyr)
source('database.r')

dateFormat <- function(date) {
	format(as.POSIXct(date), format="%Y-%m-%dT%H:%M:%S")
}

conn <- connect()

queries <- list(
				sprint_start=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.start_date AS date, sprint.end_date AS end_date, \'sprint_start\' AS type
				FROM gros.sprint
				JOIN gros.project ON sprint.project_id = project.project_id'
				,
				sprint_end=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, sprint.end_date AS date, \'sprint_end\' AS type
				FROM gros.sprint
				JOIN gros.project ON sprint.project_id = project.project_id'
				,
				rank_change=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'rank_change\' AS type
				FROM gros.issue
				LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
				JOIN gros.project ON issue.project_id = project.project_id
				WHERE issue.rank_change IS NOT NULL AND updated < sprint.end_date
				ORDER BY project.project_id, sprint.start_date, issue.updated;'
				,
				storypoint_change=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'storypoint_change\' AS type
				FROM gros.issue
				JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
				LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
				JOIN gros.project ON issue.project_id = project.project_id
				WHERE issue.story_points <> prev_issue.story_points AND issue.updated < sprint.end_date
				ORDER BY project.project_id, sprint.start_date, issue.updated;'
				,
				red_metric=
				'SELECT DISTINCT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, since_date AS date, \'red_metric\' AS type
				FROM gros.metric_value
				JOIN gros.project ON metric_value.project_id = project.project_id
				LEFT OUTER JOIN gros.sprint ON metric_value.project_id = sprint.project_id AND metric_value.since_date BETWEEN sprint.start_date AND sprint.end_date
				WHERE category = \'red\' AND EXTRACT(day FROM date - since_date) > 7'
				,
				impediment=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issue.updated AS date, \'impediment\' AS type
				FROM gros.issue
				JOIN gros.issue AS prev_issue ON issue.issue_id = prev_issue.issue_id AND issue.changelog_id = prev_issue.changelog_id+1
				LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id
				JOIN gros.project ON issue.project_id = project.project_id
				WHERE issue.impediment = TRUE AND prev_issue.impediment = FALSE'
				,
				story=
				'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, issuedata.date AS date, issuedata.end_date AS end_date, \'story\' AS type
				FROM (SELECT issue.project_id, issue.sprint_id, issue.issue_id, MIN(issue.updated) AS date, MIN(end_issue.updated) AS end_date
				FROM gros.issue
				JOIN gros.issue AS end_issue ON issue.issue_id = end_issue.issue_id AND issue.changelog_id < end_issue.changelog_id
				WHERE issue.type = 7 AND issue.status = 3 AND (end_issue.resolution = 1 OR end_issue.status = 6)
				GROUP BY issue.project_id, issue.sprint_id, issue.issue_id) AS issuedata
				LEFT OUTER JOIN gros.sprint ON issuedata.project_id = sprint.project_id AND issuedata.sprint_id = sprint.sprint_id
				JOIN gros.project ON issuedata.project_id = project.project_id'
				,
				commits=
			'SELECT project."name" AS project_name, sprint.sprint_id AS sprint_id, sprint."name" AS sprint_name, commits.commit_date AS date, \'commits\' AS type
		FROM gros.commits
			JOIN gros.sprint ON commits.commit_date BETWEEN sprint.start_date AND sprint.end_date
				JOIN gros.project ON commits.project_id = project.project_id'
				)

projects <- dbGetQuery(conn, 'SELECT project.project_id, project."name" FROM gros.project ORDER BY project.project_id')

exportCommitData <- function(data) {
	lapply(as.list(1:dim(projects)[1]), function(project) {
		project_name <- projects[project,'name']
		project_id <- projects[project,'project_id']

		path <- paste("output", project_name, sep="/")
		if (!dir.exists(path)) {
			dir.create(path)
		}

		sprints <- dbGetQuery(conn, paste('SELECT sprint.sprint_id FROM gros.sprint WHERE sprint.project_id =', project_id))
		for (sprint_id in sprints$sprint_id) {
			sprint_commits <- data[data$project_name == project_name & data$sprint_id == sprint_id,]

			filename = paste(path, paste("commits", sprint_id, "json", sep="."), sep="/")
			write(toJSON(sprint_commits), file=filename)
		}
		return(data)
	})
}

exportData <- function(data, name) {
	if (name == "commits") {
		return(exportCommitData(data))
	}
	project_data <- lapply(as.list(projects$name), function(project) {
		data[data$project_name == project,]
	})
	names(project_data) <- projects$name
	path <- paste("output", paste(name, "json", sep="."), sep="/")
	write(toJSON(project_data), file=path)
	return(data)
}

extremum <- mapply(function(query, name) {
	result <- dbGetQuery(conn, query)
	result$date = dateFormat(result$date)
	if ("end_date" %in% result) {
		result$end_date = dateFormat(result$end_date)
	}
	exportData(result, name)

	minDate = min(result$date)
	maxDate = max(result$date, result$end_date)
	return(list(min_date=minDate, max_date=maxDate))
}, query=queries, name=names(queries))

total_data = list(min_date=min(unlist(extremum['min_date',])),
				  max_date=max(unlist(extremum['max_date',])),
				  update_date=dateFormat(Sys.time()),
				  projects=projects$name)

write(toJSON(total_data), file="output/data.json")
