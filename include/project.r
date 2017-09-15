# Utilities for retrieving lists of projects.

if (!exists('INC_PROJECT_R')) {
	INC_PROJECT_R <- T

	get_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
							   ORDER BY', by))
	}

	get_main_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
							   WHERE main_project IS NULL ORDER BY', by))
	}

	get_sprint_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT DISTINCT project.project_id, project.name
							   FROM gros.project
							   JOIN gros.sprint
							   ON project.project_id = sprint.project_id
							   ORDER BY', by))
	}

	get_repo_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT DISTINCT project.project_id, project.name
							   FROM gros.project
							   JOIN gros.repo
							   ON project.project_id = repo.project_id
							   ORDER BY', by))
	}

	get_recent_projects <- function(conn, date) {
		if (missing(date)) {
			# Since the beginning of this year
			date <- as.Date(paste(format(Sys.Date(), "%Y"), "01", "01", sep="-"))
		}
		query <- paste('SELECT project.project_id, project.name
					   FROM gros.sprint
					   JOIN gros.project
					   ON sprint.project_id = project.project_id
					   WHERE start_date >= CAST(\'', date, '\' AS TIMESTAMP)
					   GROUP BY project.project_id, project.name
					   ORDER BY project.project_id', sep='')
		dbGetQuery(conn, query)
	}
}
