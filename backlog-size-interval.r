source('include/project.r')

conn <- connect()
projects <- get_projects_meta(conn,
							  fields=c('project_id', 'name', 'quality_display_name'),
							  metadata=list(core=T, main=T))
intervals <- get_project_intervals(conn)
print(projects)
projects <- projects[projects$core & projects$main & projects$project_id %in% colnames(intervals),]
print(projects)

out <- mapply(function(project_id, name, quality_name) {
	if (!is.na(quality_name)) {
		name <- quality_name
	}
	print(name)
	interval <- c(0, intervals[,as.character(project_id)])
	sizes <- lapply(seq(2, length(interval)), function(index) {
		domain <- c(interval[index-1], interval[index])
		constraint <- paste('CAST(', as.Date(domain, origin="1970-01-01"),
							' AS TIMESTAMP)', sep='\'', collapse=' AND ')
		query <- paste('SELECT COUNT(DISTINCT issue.issue_id) AS backlog_size
						FROM gros.issue
						WHERE issue.project_id =', project_id,
						'AND issue.updated BETWEEN', constraint)
		dbGetQuery(conn, query)$backlog_size
	})
	return(c(name, sizes))
}, projects$project_id, projects$name, projects$quality_display_name, SIMPLIFY=T)

write.csv(t(out), file=paste('output', 'backlog_size_interval.csv', sep='/'),
		  row.names=F, col.names=c("Project", paste("Interval", 1:4))
