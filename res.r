library(jsonlite)
source('include/args.r')

glob <- get_arg('--glob', default="output/recent_sprint_features/*")
predictor <- get_arg('--predictor', default="backlog_points")

ones <- list()
twos <- list()
for (file in Sys.glob(paste(glob, "errors.json", sep="/"))) {
    stats <- read_json(file)
    for (scenario in names(stats[[predictor]])) {
        ones[[scenario]] <- c(ones[[scenario]],
                              stats[[predictor]][[scenario]][1])
        twos[[scenario]] <- c(twos[[scenario]],
                              stats[[predictor]][[scenario]][2])
    }
    mcp1 <- paste(predictor, 'stats.1', sep='_')
    for (scenario in names(stats[[mcp1]])) {
        sc1 <- paste(scenario, 'mc', sep='_')
        ones[[sc1]] <- c(ones[[sc1]], stats[[mcp1]][[scenario]][1])
    }
    mcp2 <- paste(predictor, 'stats.2', sep='_')
    for (scenario in names(stats[[mcp2]])) {
        sc2 <- paste(scenario, 'mc', sep='_')
        twos[[sc2]] <- c(twos[[sc2]], stats[[mcp2]][[scenario]][1])
    }
}
for (scenario in names(ones)) {
    print(scenario)
    ones[[scenario]][ones[[scenario]] == "NA"] <- NA
    twos[[scenario]][twos[[scenario]] == "NA"] <- NA
    ones[[scenario]] <- unlist(ones[[scenario]])
    twos[[scenario]] <- unlist(twos[[scenario]])
    str(ones[[scenario]])
    str(twos[[scenario]])
    print(paste(' one third =', mean(ones[[scenario]], na.rm=T), '+/-',
                sd(ones[[scenario]], na.rm=T)))
    print(paste('two thirds =', mean(twos[[scenario]], na.rm=T), '+/-',
                sd(twos[[scenario]], na.rm=T)))
}
