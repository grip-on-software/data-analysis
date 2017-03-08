# R script that extracts features regarding sprints from the database and
# exports them to an ARFF file readable by Weka and other data mining tools.

library(foreign) # For write.arff
source('database.r')
source('sprint_features.r')
conn <- connect()

result <- get_sprint_features(conn)
sprint_data <- result$data

write.arff(sprint_data[,result$colnames], file='output/sprint_features.arff',
		   relation="sprint_data")
