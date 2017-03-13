source('database.r')
source('log.r')

export <- function(items, prefix, field) {
	for (item in items) {
		name <- item[[field]]
		write(item$query, file=paste('output', paste(name, 'sql', sep='.'), sep='/'))
	}
}

export(load_queries('sprint_features.yml', 'sprint_definitions.yml'), 'feature', 'table')
export(load_queries('sprint_events.yml', 'sprint_definitions.yml'), 'event', 'type')
