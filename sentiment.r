# Script to output sentiment analysis values for issue tracker comments
# Requires pattern.nlp package - https://github.com/bnosac/pattern.nlp
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(pattern.nlp)
library(foreign)
library(ggplot2)
library(jsonlite)
library(zoo)
source('include/args.r')
source('include/database.r')
source('include/log.r')

make_opt_parser(desc="Output sentiment values for issue tracker comments",
                options=list(make_option('--output', default='output',
                                         help='Output directory'),
                             make_option('--plot', action='store_true',
                                         default=F, help='Create a plot'),
                             make_option('--load', action='store_true',
                                         default=F,
                                         help='Load sentiments from CSV file')))
config <- get_config()
arguments <- config$args
log_setup(arguments)

output_directory <- arguments$output
plot <- arguments$plot

collect <- function() {
    conn <- connect()
    patterns <- load_definitions('sprint_definitions.yml')
    message <- paste('*Resolving* as a result of the',
                     '*Resolve* action being applied to the parent.')
    query <- paste("SELECT project.project_id, project.name, sprint.sprint_id,
                    sprint.start_date, comment.message
                    FROM gros.comment
                    JOIN gros.issue
                    ON issue.issue_id = comment.issue_id
                    AND issue.changelog_id = 0
                    JOIN gros.project
                    ON issue.project_id = project.project_id
                    JOIN gros.sprint
                    ON sprint.project_id = project.project_id
                    AND comment.date BETWEEN ${s(sprint_open)} AND
                        ${s(sprint_close)}
                    WHERE project.is_support_team = false
                    AND message NOT LIKE 'Versie%\nGeslaagd:%'
                    AND message <> '", pmessage, "'
                    ORDER BY project.name, sprint.sprint_id", sep='')
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
if (arguments$load) {
    loginfo('loading from %s', path)
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
if (plot) {
    boxplot(as.numeric(as.character(res$polarity)) ~ res$project_id, data=res)
}

project_sentiment <-  function(project) {
    polarity <- as.numeric(as.character(project$polarity))
    by <- list(start_date=as.Date(project$start_date,
                                  '%Y-%m-%d'),
               sprint_id=project$sprint_id)
    values <- aggregate(list(polarity=polarity), by, mean)
    values$project_id <- project[[1, 'project_id']]
    values$sprint_num <- rownames(values)
    if (dim(values)[1] >= 3) {
        values$polarity <- rollmean(values$polarity, 3, fill=0)
    }
    if (plot) {
        aspect_ratio <- 1/1.6
        date <- values$start_date
        print(date)
        print(values$sprint_num)
        ggplot(values, aes(x=date, y=values$polarity)) +
            geom_line() +
            geom_hline(yintercept=0, colour='grey') +
            coord_equal(ratio=aspect_ratio) +
            labs(title=paste("Sentiment for project", project[[1, 'name']]),
                  x="Sprint date", y="Mean sentiment polarity") +
            theme(plot.title=element_text(hjust=0.5), aspect.ratio=aspect_ratio)
        ggsave(paste(output_directory,
                      paste('sentiment', project[[1, 'name']], 'png', sep='.'),
                      sep='/'))
    }
    return(values)
}

projects <- lapply(split(res, res$name), project_sentiment)

columns <- c('project_id', 'sprint_num', 'polarity')
sentiment_data <- do.call(rbind, projects)[, columns]
features <- paste(output_directory, 'sprint_features.arff', sep='/')
sprint_data <- read.arff(features)
sprint_data <- merge(sprint_data, sentiment_data,
                     by=c('project_id', 'sprint_num'), all.x=T)
write.arff(sprint_data,
           file=paste(output_directory, 'sprint_features-sentiments.arff',
                         sep='/'),
           relation="sprint_data")
