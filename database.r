# Utility file for functions that allow accessing and querying the database

if (!exists('INC_DATABASE_R')) {
	INC_DATABASE_R <- T

	library(MonetDB.R)
	library(DBI)
	library(digest)
	library(stringr)
	library(yaml)

	connect <- function() {
		config <- yaml.load_file("config.yml")
		dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
			  	  user=config$db$user, password=config$db$password)
	}

	load_queries <- function(specification_file, definition_file) {
		data <- yaml.load_file(specification_file)
		definitions <- yaml.load_file(definition_file)
		patterns <- c(lapply(definitions$fields, function(define) { define$field }),
					  lapply(definitions$conditions, function(define) { define$condition }))

		lapply(data$files, function(item) {
			if (!is.null(item$definition)) {
				fields <- list()
				for (field in c("project_id", "sprint_id")) {
					fields <- c(fields, paste(item$table, field, sep="."))
				}
				define <- definitions$fields[[item$definition]]
				fields <- c(fields,
							paste(define$field, "AS", item$column, sep=" "))
				item$query <- paste('SELECT', paste(fields, collapse=", "),
									'FROM', paste('gros', item$table, sep='.'))
			}
			else {
				path <- paste(data$path, item$filename, sep="/")
				query <- paste(readLines(path, encoding="UTF-8"), collapse="\n")
				item$query <- str_interp(query, patterns)
			}
			return(item)
		})
	}
}
