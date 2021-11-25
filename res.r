library(jsonlite)
library(ggplot2)
if ("qqplotr" %in% rownames(installed.packages())) {
    library(qqplotr)
    QQPLOTR <- T
} else {
    QQPLOTR <- F
}

source('include/args.r')
source('include/database.r')
source('include/log.r')

glob <- get_arg('--glob', default="output/recent_sprint_features/")
predictor <- get_arg('--predictor', default="backlog_points")

ones <- list()
twos <- list()
counts <- list()
count <- 10000
stat_projects <- list()
sort_by <- list()
default_features <- paste(c("backlog_points", "velocity_three",
                            "number_of_devs"), collapse=',')
project_features <- strsplit(get_arg('--features', default=default_features),
                             ',')[[1]]
discrete <- get_arg('--discrete', default=F)
sprint_features <- load_queries('sprint_features.yml', static=T)
if (length(project_features) == 0) {
    description <- "Project"
    sort_feature <- ""
} else {
    sort_feature <- project_features[1]
    description <- paste("Project (ordered by ", sort_feature, ")", sep="")
    for (item in sprint_features) {
        if (sort_feature %in% item$column) {
            description <- paste("Project (ordered by ",
                                 tolower(item$descriptions$en), ")", sep="")
            break
        }
    }
}

for (dir in Sys.glob(glob)) {
    loginfo('Directory: %s', dir)

    projects <- read_json(paste(dir, "projects_meta.json", sep="/"))
    features <- read_json(paste(dir, "features.json", sep="/"))
    default_features <- project_features[project_features %in% features$meta |
                                         project_features %in% features$default]
    other_features <- project_features[project_features %in% features$all &
                                       !(project_features %in% default_features)]
    print(default_features)
    print(other_features)
    for (project in projects) {
        if (project$num_sprints == 0) {
            next
        }
        # Print some project stats/features
        loginfo('Project: %s (%d sprints)', project$name, project$num_sprints)
        default <- read_json(paste(dir, project$name, "default.json", sep="/"))
        all_zero <- length(project_features) > 0
        features <- default[[length(default)]][default_features]
        other_data <- list()
        for (feature in other_features) {
            other_data[[feature]] <- read_json(paste(dir, project$name,
                                                     paste(feature, "json",
                                                           sep="."),
                                                     sep="/"), auto_unbox=T)
            features[[feature]] <- other_data[[feature]][[length(default)]]
        }
        for (sprint in seq(1, length(default))) {
            loginfo('%s: %s', default_features,
                    default[[sprint]][default_features])
            if (any(default[[sprint]][default_features] != 0)) {
                all_zero <- F
            }
            for (feature in other_features) {
                if (is.null(other_data[[feature]][[sprint]])) {
                    next
                }
                loginfo('%s: %f', feature, other_data[[feature]][[sprint]])
                if (other_data[[feature]][[sprint]] != 0) {
                    all_zero <- F
                }
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
            sort_by[[scenario]] <- c(sort_by[[scenario]],
                                     list(features[[sort_feature]]))
        }
        mcp1 <- paste(predictor, 'stats.1', sep='_')
        for (scenario in names(stats[[mcp1]])) {
            sc1 <- paste(scenario, 'mc', sep='_')
            ones[[sc1]] <- c(ones[[sc1]], stats[[mcp1]][[scenario]][1])
            stat_projects[[sc1]] <- c(stat_projects[[sc1]], project$name)
            sort_by[[sc1]] <- c(sort_by[[sc1]], list(features[[sort_feature]]))
        }
        mcp2 <- paste(predictor, 'stats.2', sep='_')
        for (scenario in names(stats[[mcp2]])) {
            sc2 <- paste(scenario, 'mc', sep='_')
            twos[[sc2]] <- c(twos[[sc2]], stats[[mcp2]][[scenario]][1])
        }
        mcc <- paste(predictor, 'counts', sep='_')
        for (scenario in names(stats[[mcc]])) {
            counts[[scenario]] <- c(counts[[scenario]],
                                    as.numeric(unlist(stats[[mcc]][[scenario]])))
        }

        for (scenario in names(ones)) {
            loginfo('Scenario: %s', scenario)
            loginfo('one third = %s, two thirds = %s',
                    ones[[scenario]][length(ones[[scenario]])],
                    twos[[scenario]][length(twos[[scenario]])])
        }
    }
}

qqplot_monte_carlo <- function(counts, file) {
    df <- data.frame(sample=colMeans(counts, na.rm=T),
                     ymin=sapply(counts, min, na.rm=T),
                     ymax=sapply(counts, max, na.rm=T))
    ggplot(data=df, aes(sample=df$sample)) +
        stat_qq_line(colour="#3366FF") +
        stat_qq_point() +
        labs(x="Theoretical Quantiles", y="Sample Quantiles")
    ggsave(file)
}

plot_scenario <- function(data, x, y, title, file, limits) {
    plot <- ggplot(data, aes(x=data[[x$column]], y=data[[y$column]])) +
        geom_point()
    if (!x$discrete) {
        plot <- plot + geom_smooth(method="lm", se=F)
    }
    plot <- plot + x$scale +
        scale_y_continuous("Error", limits=limits)
    ggsave(file)
}

limits <- list(
    velocity_three=c(-2000, 0),
    backlog_all_velocity_three=c(-2000, 0),
    velocity_three_mc=c(-1200, 600),
    backlog_all_velocity_three_mc=c(-1200, 600),
    backlog_all_velocity_three_sep_mc=c(-1200, 600)
)

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

    x <- list(discrete=discrete,
              column=ifelse(length(project_features) == 0,
                            "projects", "sort_by"))
    if (discrete || length(project_features) == 0) {
        x$scale <- scale_x_discrete(description)
        sort <- as.character(sort_by[[scenario]])
    } else {
        x$scale <- scale_x_continuous(description)
        sort <- as.numeric(sort_by[[scenario]])
    }

    data <- data.frame(ones=ones[[scenario]],
                       twos=twos[[scenario]],
                       projects=stat_projects[[scenario]],
                       sort_by=sort)
    print(data)

    plot_scenario(data, x=x, y=list(column="ones"),
                  title=paste("One third (", scenario, ")", sep=""),
                  file=paste(scenario, "one_third", "pdf", sep="."),
                  limits=limits[[scenario]])

    plot_scenario(data, x=x, y=list(column="twos"),
                  title=paste("Two thirds (", scenario, ")", sep=""),
                  file=paste(scenario, "two_thirds", "pdf", sep="."),
                  limits=limits[[scenario]])
}

for (scenario in names(counts)) {
    if (QQPLOTR) {
        qqplot_monte_carlo(matrix(counts[[scenario]], ncol=count, byrow=T),
                           paste("qqplot", scenario, "pdf", sep="."))
    } else {
        write.table(counts[[scenario]],
                    paste("qqplot", scenario, "txt", sep="."),
                    row.names=F, col.names=F)
    }
}
