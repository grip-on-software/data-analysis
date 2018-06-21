# Utilities for retrieving lists of projects.

if (!exists('INC_PROJECT_R')) {
	INC_PROJECT_R <- T

	get_projects_meta <- function(conn, fields=c('project_id', 'name'),
								  metadata=list(), by='project_id') {
		joins <- list()
		aliases <- list()
		groups <- c()
		must_group <- F

		if (!is.null(metadata$recent)) {
			if (isTRUE(metadata$recent)) {
				# Within the last three months
				date <- Sys.Date() - as.difftime(12, units="weeks")
			}
			else {
				date <- as.Date(metadata$recent)
			}
			aliases$recent <- paste('MAX(start_date) >= CAST(', date, ' AS TIMESTAMP)', sep='\'')
			joins$sprint <- 'sprint.project_id = project.project_id'
			must_group <- T
		}
		if (!is.null(metadata$core)) {
			aliases$core <- 'NOT COALESCE(is_support_team, false)'
			groups <- c(groups, 'is_support_team')
		}
		if (!is.null(metadata$main)) {
			aliases$main <- 'CASE WHEN main_project IS NULL THEN true ELSE false END'
			groups <- c(groups, 'main_project')
		}

		if (length(joins) > 0) {
			fields <- paste('project', fields, sep='.')
			groups <- paste('project', groups, sep='.')
			by <- paste('project', by, sep='.')
		}
		group_by <- ''
		if (must_group) {
			group_by <- paste('GROUP BY', paste(c(fields, groups),
												collapse=', '))
		}
		fields <- c(fields, mapply(function(alias, expression) {
			paste(expression, 'AS', alias)
 	 	}, names(aliases), aliases))

		query <- paste('SELECT', paste(fields, collapse=', '),
					   'FROM gros.project',
					   paste(mapply(function(table, cond) {
					         paste('JOIN', paste('gros', table, sep='.'),
							 	   'ON', cond)
					   }, names(joins), joins), collapse=' '),
					   group_by,
					   'ORDER BY', paste(by, collapse=', '))
		dbGetQuery(conn, query)
	}

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

	get_meta_keys <- function(project_metadata) {
		meta_keys <- strsplit(project_metadata, ',')[[1]]
		metadata <- vector("list", length(meta_keys))
		names(metadata) <- meta_keys
		metadata[] <- T
		return(metadata)
	}

	write_projects_metadata <- function(conn, fields, metadata, projects=NA,
										project_ids='0',
										output_directory='output') {
		if (is.na(projects)) {
			projects <- get_projects_meta(conn, fields=fields, 
										  metadata=metadata)
		}
		if (project_ids != '0') {
			projects$name <- paste('Proj', projects$project_id, sep='')
			projects$quality_display_name <- NULL
			projects <- projects[order(projects$project_id),]
		}
		else {
			projects <- projects[order(projects$name),]
		}
		projects$project_id <- NULL
		write(toJSON(projects, auto_unbox=T),
		  	  file=paste(output_directory, 'projects_meta.json', sep='/'))
	}
}
