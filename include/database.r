# Utility file for functions that allow accessing and querying the database

if (!exists('INC_DATABASE_R')) {
	INC_DATABASE_R <- T

	library(MonetDB.R)
	library(DBI)
	library(digest)
	library(stringr)
	library(yaml)
	source('include/args.r')

	connect <- function() {
		config <- yaml.load_file("config.yml")
		dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
				  user=config$db$user, password=config$db$password)
	}

	load_definitions <- function(definition_file, variables) {
		definitions <- yaml.load_file(definition_file)
		if (missing(variables)) {
			variables <- NULL
		}
		else {
			for (name in names(variables)) {
				arg <- get_arg(paste('--', gsub('_', '-', name), sep=''),
							   default=variables[[name]])
				variables[[name]] <- arg
			}
		}

		patterns <- c(lapply(definitions$fields,
					  		 function(define) { define$field }),
					  lapply(definitions$conditions,
					  		 function(define) { define$condition }),
					  variables)
		recursive_str_interp <- function(string, ...) {
			str_interp(string, c(patterns, list(...)))
		}
		patterns <- c(patterns, list(s=recursive_str_interp))

		return(patterns)
	}

	load_query <- function(item, patterns, path) {
		if (!is.null(item$definition)) {
			fields <- list()
			for (field in c("project_id", "sprint_id")) {
				fields <- c(fields, paste(item$table, field, sep="."))
			}
			define <- patterns[[item$definition]]
			fields <- c(fields, paste(define, "AS", item$column, sep=" "))
			item$query <- paste('SELECT', paste(fields, collapse=", "),
								'FROM', paste('gros', item$table, sep='.'))
		}
		else {
			if (!is.null(item$query)) {
				query <- item$query
			}
			else {
				path <- paste(path, item$filename, sep="/")
				query <- paste(readLines(path, encoding="UTF-8"), collapse="\n")
			}
			item$query <- str_interp(query, patterns)
			item$patterns <- patterns
		}
		return(item)
	}

	load_queries <- function(specification_file, definition_file, variables) {
		data <- yaml.load_file(specification_file)
		patterns <- load_definitions(definition_file, variables)

		lapply(data$files, load_query, patterns, data$path)
	}
}
