library(MonetDB.R)
library(DBI)
library(digest)
library(yaml)

connect <- function() {
	config <- yaml.load_file("config.yml")
	dbConnect(MonetDB.R(), host=config$db$host, dbname=config$db$dbname,
			  user=config$db$user, password=config$db$password)
}
