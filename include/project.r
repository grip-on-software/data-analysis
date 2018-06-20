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

	get_subprojects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name, main_project
							   FROM gros.project WHERE main_project IS NOT NULL
							   ORDER BY', by))
	}

	get_core_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name FROM gros.project
							   WHERE COALESCE(is_support_team, false) = false
							   ORDER BY', by))
	}

	get_sprint_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name FROM
							   (SELECT DISTINCT project.project_id, project.name
							   FROM gros.project
							   JOIN gros.sprint
							   ON project.project_id = sprint.project_id)
							   AS sprint_project
							   ORDER BY', by))
	}

	get_repo_projects <- function(conn, by='project_id') {
		dbGetQuery(conn, paste('SELECT project_id, name FROM
							   (SELECT DISTINCT project.project_id, project.name
							   FROM gros.project
							   JOIN gros.repo
							   ON project.project_id = repo.project_id)
							   AS repo_project
							   ORDER BY', by))
	}

	get_recent_projects <- function(conn, date) {
		# Retrieve projects that have had a sprint recently
		if (missing(date)) {
			# Within the last three months
			date <- Sys.Date() - as.difftime(12, units="weeks")
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
