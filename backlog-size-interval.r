# Script to generate CSV file containing a measured backlog size between certain
# intervals defined by project length.

source('include/log.r')
source('include/project.r')

conn <- connect()
config <- get_config()
patterns <- load_definitions('sprint_definitions.yml', config$fields)
output_directory <- get_arg('--output', default='output')
count <- get_arg('--count', default=4)

projects <- get_projects_meta(conn,
							  fields=c('project_id', 'name', 'quality_display_name'),
							  metadata=list(core=T, main=T))
intervals <- get_project_intervals(conn, count)
projects <- projects[projects$core & projects$main & projects$project_id %in% colnames(intervals),]

out <- mapply(function(project_id, name, quality_name) {
	if (!is.na(quality_name)) {
		name <- quality_name
	}
	loginfo(paste("Calculating values at intervals for", name))
	interval <- c(0, intervals[,as.character(project_id)])
	sizes <- lapply(seq(2, length(interval)), function(index) {
		domain <- c(interval[index-1], interval[index])
		constraint <- paste('CAST(', as.Date(domain, origin="1970-01-01"),
							' AS TIMESTAMP)', sep='\'', collapse=' AND ')
		query <- paste('SELECT COUNT(DISTINCT issue.issue_id) AS backlog_size
						FROM gros.issue
						WHERE issue.project_id =', project_id,
						'AND issue.updated BETWEEN', constraint,
						'AND ${issue_not_done}')
		item <- load_query(list(query=query), patterns)
		dbGetQuery(conn, item$query)$backlog_size
	})
	return(c(name, sizes))
}, projects$project_id, projects$name, projects$quality_display_name, SIMPLIFY=T)

write.table(t(out), sep=",",
			file=paste(output_directory, 'backlog_size_interval.csv', sep='/'),
			row.names=F, col.names=c("Project", paste("Interval", 1:count)))
