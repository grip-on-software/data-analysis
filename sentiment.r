source('include/database.r')
library(pattern.nlp)

conn <- connect()
res <- dbGetQuery(conn, "SELECT message FROM gros.comment JOIN gros.issue ON issue.issue_id = comment.issue_id AND issue.changelog_id = 0 JOIN gros.project ON issue.project_id = project.project_id WHERE project.is_support_team = false AND message NOT LIKE 'Versie%\nGeslaagd:%' ORDER BY RAND() LIMIT 500")

for (message in res$message) {
	print(pattern_sentiment(message, language="dutch"))
}
