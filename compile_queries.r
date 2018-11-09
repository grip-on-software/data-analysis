source('include/database.r')
source('include/log.r')

export <- function(items, prefix, field) {
    for (item in items) {
        name <- item[[field]]
        write(item$query,
              file=paste('output', paste(paste(prefix, name, sep='_'),
                           'sql', sep='.'), sep='/'))
    }
}

project_ids <- get_arg('--project-ids', default='0')
definitions <- yaml.load_file('analysis_definitions.yml')
analysis_definitions <- c(lapply(definitions$fields,
                                 function(define) { define$field }),
                          list(project_ids=project_ids))

export(load_queries('sprint_features.yml', 'sprint_definitions.yml',
                    list(sprint_conditions='')),
       'feature', 'table')
export(load_queries('sprint_events.yml', 'sprint_definitions.yml',
                    list(project_ids=project_ids)),
       'event', 'type')
export(load_queries('analysis_reports.yml', 'sprint_definitions.yml',
                    analysis_definitions),
       'report', 'table')
export(load_queries('project_features.yml', 'sprint_definitions.yml'),
       'project', 'column')
