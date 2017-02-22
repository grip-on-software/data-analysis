# Utility file for functions that allow accessing and querying the database

library(MonetDB.R)
library(DBI)
library(digest)
library(yaml)

connect <- function() {
	config <- yaml.load_file("config.yml")
	dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
			  user=config$db$user, password=config$db$password)
}

load_queries <- function(specification_file) {
	data <- yaml.load_file(specification_file)
	lapply(data$files, function(item) {
		path <- paste(data$path, item$filename, sep="/")
		item$query = paste(readLines(path, encoding="UTF-8"), collapse="\n")
		item
	})
}
