library(jsonlite)
library(ggplot2)
source('include/args.r')
source('include/log.r')

glob <- get_arg('--glob', default="output/recent_sprint_features/")
predictor <- get_arg('--predictor', default="backlog_points")

ones <- list()
twos <- list()
stat_projects <- list()
default_features <- paste(c("backlog_points", "velocity_three",
                            "number_of_devs"), collapse=',')
project_features <- strsplit(get_arg('--features', default=default_features),
                             ',')[[1]]

for (dir in Sys.glob(glob)) {
    loginfo('Directory: %s', dir)

    projects <- read_json(paste(dir, "projects_meta.json", sep="/"))
    features <- read_json(paste(dir, "features.json", sep="/"))
    default_features <- project_features[project_features %in% features$default]
    other_features <- project_features[project_features %in% features$all &
                                       !(project_features %in% features$default)]
    print(default_features)
    print(other_features)
    for (project in projects) {
        if (project$num_sprints == 0) {
            next
        }
        # Print some project stats/features
        loginfo('Project: %s (%d sprints)', project$name, project$num_sprints)
        default <- read_json(paste(dir, project$name, "default.json", sep="/"))
        all_zero <- T
        for (sprint in seq(1, length(default))) {
            loginfo('%s: %s', default_features,
                    default[[sprint]][default_features])
            if (any(default[[sprint]][default_features] != 0)) {
                all_zero <- F
            }
            for (feature in other_features) {
                data <- read_json(paste(dir, project$name,
                                        paste(feature, "json", sep="."),
                                        sep="/"), auto_unbox=T)
                loginfo('%s: %f', feature, data[[sprint]])
            }
        }
        if (all_zero) {
            loginfo('Skipping project')
            next
        }
        stats <- read_json(paste(dir, project$name, "errors.json", sep="/"))
        for (scenario in names(stats[[predictor]])) {
            ones[[scenario]] <- c(ones[[scenario]],
                                  stats[[predictor]][[scenario]][1])
            twos[[scenario]] <- c(twos[[scenario]],
                                  stats[[predictor]][[scenario]][2])
            stat_projects[[scenario]] <- c(stat_projects[[scenario]],
                                           project$name)
        }
        mcp1 <- paste(predictor, 'stats.1', sep='_')
        for (scenario in names(stats[[mcp1]])) {
            sc1 <- paste(scenario, 'mc', sep='_')
            ones[[sc1]] <- c(ones[[sc1]], stats[[mcp1]][[scenario]][1])
            stat_projects[[sc1]] <- c(stat_projects[[sc1]], project$name)
        }
        mcp2 <- paste(predictor, 'stats.2', sep='_')
        for (scenario in names(stats[[mcp2]])) {
            sc2 <- paste(scenario, 'mc', sep='_')
            twos[[sc2]] <- c(twos[[sc2]], stats[[mcp2]][[scenario]][1])
        }

        for (scenario in names(ones)) {
            loginfo('Scenario: %s', scenario)
            loginfo('one third = %s, two thirds = %s',
                    ones[[scenario]][length(ones[[scenario]])],
                    twos[[scenario]][length(twos[[scenario]])])
        }
    }
}
for (scenario in names(ones)) {
    print(scenario)
    ones[[scenario]][ones[[scenario]] == "NA"] <- NA
    twos[[scenario]][twos[[scenario]] == "NA"] <- NA
    ones[[scenario]] <- unlist(ones[[scenario]])
    twos[[scenario]] <- unlist(twos[[scenario]])
    mu1 <- mean(ones[[scenario]], na.rm=T)
    sigma1 <- sd(ones[[scenario]], na.rm=T)
    mu2 <- mean(twos[[scenario]], na.rm=T)
    sigma2 <- sd(twos[[scenario]], na.rm=T)
    print(paste(' one third =', mu1, '+/-', sigma1))
    print(paste('two thirds =', mu2, '+/-', sigma2))

    o <- data.frame(ones[scenario])
    t <- data.frame(twos[scenario])
    s <- data.frame(stat_projects[scenario], stringsAsFactors=F)
    str(o)
    str(t)

    ggplot(o, aes(x=s[[scenario]], y=o[[scenario]])) +
        geom_boxplot() +
        scale_x_discrete("Project") +
        scale_y_continuous("Error") +
        labs(title=paste("One third (", scenario, ")", sep=""))
    ggsave(paste(scenario, "one_third", "pdf", sep="."))
    ggplot(t, aes(x=s[[scenario]], y=t[[scenario]])) +
        geom_boxplot() +
        scale_x_discrete("Project") +
        scale_y_continuous("Error") +
        labs(title=paste("Two thirds (", scenario, ")", sep=""))
    ggsave(paste(scenario, "two_thirds", "pdf", sep="."))
}
