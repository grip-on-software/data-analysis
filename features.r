# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
source('include/args.r')
source('include/database.r')
source('include/sprint_features.r')
conn <- connect()

exclude <- get_arg('--exclude')
result <- get_sprint_features(conn, exclude)
sprint_data <- result$data

write.arff(sprint_data[,result$colnames], file='output/sprint_features.arff',
		   relation="sprint_data")
