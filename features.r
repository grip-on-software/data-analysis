# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
library(jsonlite)
source('include/args.r')
source('include/database.r')
source('include/sprint_features.r')
conn <- connect()

exclude <- get_arg('--exclude', '^$')
if (get_arg('--project', F)) {
	result <- get_project_features(conn, exclude)
	df <- result$data[,result$colnames]
	data <- lapply(as.list(split(df, seq(nrow(df)))), unbox)
	names(data) <- result$data[['name']]
	write(toJSON(data), file="output/project_features.json")
} else {
	result <- get_sprint_features(conn, exclude)
	sprint_data <- result$data

	write.arff(sprint_data[,result$colnames], file='output/sprint_features.arff',
		   	   relation="sprint_data")
}
