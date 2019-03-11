# Analysis reports.

library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/log.r')
source('include/analysis_reports.r')

conn <- connect()

# 'each', 'main', empty or a comma-separated list of projects
projects_list <- get_arg('--projects', default='')
invert <- get_arg('--invert', default=F)

interval <- get_arg('--interval', default='')
latest_date <- as.POSIXct(get_arg('--latest-date', default=Sys.time()))

report <- get_arg('--report', default='.*')

output_directory <- get_arg('--output', default='output')

project_ids <- get_arg('--project-ids', default='0')
if (project_ids != '0') {
    project_ids <- '1'
}

config <- get_config()

project_metadata <- get_arg('--project-metadata', default='recent,core,main')
metadata <- get_meta_keys(project_metadata)

join_cols <- c('project_id')
fields <- list('project_id', 'name', 'quality_display_name')
names(fields) <- fields
if (config$db$primary_source == "tfs") {
    join_cols <- c('team_id')
    fields$project_id <- "team_id"
    fields$quality_display_name <- NULL
}

project_sources <- strsplit(get_arg('--project-sources', default=''), ',')[[1]]

run_reports <- function(definitions) {
    reports <- get_analysis_reports(definitions, latest_date)

    for (item in reports$items) {
        if (length(grep(report, item$table)) > 0) {
            loginfo('Executing query for report %s', item$table)
            logdebug(item$query)
            time <- system.time(result <- dbGetQuery(conn, item$query))
            loginfo('Query for report %s took %f seconds', item$table,
                    time['elapsed'])
            time <- system.time(item$report(item, result, output_directory))
            loginfo('Generation of report %s took %f seconds', item$table,
                    time['elapsed'])
        }
    }
    return(reports)
}

if (interval != '') {
    # Always run the full report, this will also empty the output directory
    reports <- run_reports(list(id='all', project_ids=project_ids))

    item <- load_query(list(query='SELECT MIN(updated) AS start_date
                                   FROM gros.${t("issue")}
                                   WHERE assignee IS NOT NULL'),
                       reports$patterns)

    start_date <- dbGetQuery(conn, item$query)[[1]]
    intervals <- seq(as.POSIXct(start_date), latest_date, by=interval)
    loginfo(intervals)
    rollapply(intervals, 2,
              function(range) {
                  id <- paste('interval', as.numeric(range[1]), sep='-')
                  condition <- paste('WHERE ${field} BETWEEN ',
                                     'epoch(', as.numeric(range[1]), ') AND ',
                                     'epoch(', as.numeric(range[2]), ')',
                                     sep='')
                  run_reports(list(id=id,
                                   project_ids=project_ids,
                                   interval_condition=condition))
              })

    output_directory <- get_arg('--output', default='output')
    write(toJSON(head(as.numeric(intervals), n=-1)),
          file=paste(output_directory, "intervals.json", sep="/"))
} else if (projects_list == '') {
    reports <- run_reports(list(id='all', project_ids=project_ids))
    write_projects_metadata(conn, fields, metadata, projects=NA,
                            project_ids=project_ids,
                            project_sources=project_sources,
                            output_directory=output_directory,
                            patterns=reports$patterns,
                            join_cols=join_cols)
} else {
    patterns <- load_definitions('sprint_definitions.yml', list())
    projects <- get_projects_meta(conn, fields=fields, metadata=metadata,
                                  patterns=patterns, join_cols=join_cols)
    if (projects_list == 'main') {
        projects <- projects[projects$main, ]
    }
    else if (projects_list != 'each') {
        ids <- as.vector(as.numeric(unlist(strsplit(projects_list, ','))))
        projects <- projects[projects$project_id %in% ids, ]
    }
    if (project_ids != '0') {
        projects$name <- paste('Proj', projects$project_id, sep='')
    }
    mapply(function(project_id, name) {
               condition <- paste('AND', join_cols[1])
               run_reports(list(id=project_id,
                                name=name,
                                project_ids=project_ids,
                                category_conditions=paste(condition, '=',
                                                          project_id)))
               if (invert) {
                   run_reports(list(id=paste('not', project_id, sep='-'),
                                    name=paste('not', name, sep='-'),
                                    project_ids=project_ids,
                                    category_conditions=paste(condition, '<>',
                                                              project_id)))
               }
           },
           projects$project_id, projects$name, SIMPLIFY=F)
    write_projects_metadata(conn, fields, metadata, projects=projects,
                            project_ids=project_ids,
                            project_sources=project_sources,
                            output_directory=output_directory)
    write(toJSON(projects$project_id),
          file=paste(output_directory, 'report_projects.json', sep='/'))
    write(toJSON(projects$name),
          file=paste(output_directory, 'report_project_names.json', sep='/'))
}
