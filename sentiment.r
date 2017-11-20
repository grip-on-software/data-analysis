source('include/args.r')
source('include/database.r')
library(pattern.nlp)
library(ggplot2)
library(zoo)

output_directory <- get_arg('--output', default='output')

collect <- function() {
	conn <- connect()
	patterns <- load_definitions('sprint_definitions.yml')
	query <- "SELECT project.name, sprint.sprint_id, sprint.start_date, comment.message FROM gros.comment JOIN gros.issue ON issue.issue_id = comment.issue_id AND issue.changelog_id = 0 JOIN gros.project ON issue.project_id = project.project_id LEFT JOIN gros.sprint ON issue.project_id = project.project_id AND comment.date BETWEEN sprint.start_date AND ${sprint_close} WHERE project.is_support_team = false AND message NOT LIKE 'Versie%\nGeslaagd:%' AND message <> '*Resolving* as a result of the *Resolve* action being applied to the parent.' ORDER BY project.name, sprint.sprint_id"
	item <- load_query(list(query=query), patterns)
	res <- dbGetQuery(conn, item$query)

	res$polarity <- 0
	res$subjectivity <- 0

	res <- as.data.frame(t(apply(res, 1, function(row) {
		sentiment <- pattern_sentiment(row[['message']], language="dutch")
		row['polarity'] <- sentiment$polarity
		row['subjectivity'] <- sentiment$subjectivity
		return(row)
	})))

	res$message <- NULL

	return(res)
}

export <- function(path, res) {
	write.table(res, file=path, row.names=F, sep=",")
}

load <- function(path) {
	read.table(path, header=T, sep=",")
}

path <- paste(output_directory, 'sentiment.csv', sep='/')
if (get_arg('--load', default=F)) {
	print('loading')
	res <- load(path)
} else {
	res <- collect()
	export(path, res)
}

stats <- function(x) {
	c(mean=mean(x),
	  nonzero=length(x != 0),
	  var=var(x != 0),
	  min=min(x),
	  max=max(x))
}

print(aggregate(as.numeric(as.character(res$polarity)), list(res$name), stats))
boxplot(as.numeric(as.character(res$polarity)) ~ res$name, data=res)

invisible(lapply(split(res, res$name), function(project) {
	values <- aggregate(list(polarity=as.numeric(as.character(project$polarity))), list(start_date=project$start_date), mean)
	#indexes <- which(values$polarity != 0, arr.ind=T)
	#print(indexes)
	#data <- values[indexes,]
	values$polarity <- rollmean(values$polarity, 10, fill=0)
	print(values)
	aspect_ratio = 1/1.6
	date <- as.Date(values$start_date, '%Y-%m-%d')
	ggplot(values, aes(x=date, y=values$polarity)) +
		geom_line() +
		geom_hline(yintercept=0, colour='grey') +
		coord_equal(ratio=aspect_ratio) +
		labs(title=paste("Sentiment for project", project[[1, 'name']]),
			 x="Sprint date", y="Mean sentiment polarity") +
		theme(plot.title=element_text(hjust=0.5), aspect.ratio=aspect_ratio)
	ggsave(paste(output_directory,
				 paste('sentiment', project[[1,'name']], 'png', sep='.'),
				 sep='/'))
}))
