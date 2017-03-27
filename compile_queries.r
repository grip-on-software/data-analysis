source('database.r')
source('log.r')

export <- function(items, prefix, field) {
	for (item in items) {
		name <- item[[field]]
		write(item$query,
			  file=paste('output', paste(paste(prefix, name, sep='_'),
			  			 'sql', sep='.'), sep='/'))
	}
}

definitions <- yaml.load_file('analysis_definitions.yml')
analysis_definitions <- lapply(definitions$fields,
							   function(define) { define$field })

export(load_queries('sprint_features.yml', 'sprint_definitions.yml'),
	   'feature', 'table')
export(load_queries('sprint_events.yml', 'sprint_definitions.yml'),
	   'event', 'type')
export(load_queries('analysis_reports.yml', 'sprint_definitions.yml',
					analysis_definitions),
	   'report', 'table')
