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
}
for (scenario in names(ones)) {
    print(scenario)
    ones[[scenario]][ones[[scenario]] == "NA"] <- NA
    twos[[scenario]][twos[[scenario]] == "NA"] <- NA
    ones[[scenario]] <- unlist(ones[[scenario]])
    twos[[scenario]] <- unlist(twos[[scenario]])
    str(ones[[scenario]])
    str(twos[[scenario]])
    print(paste('ones = ', mean(ones[[scenario]], na.rm=T),
                sd(ones[[scenario]], na.rm=T)))
    print(paste('twos = ', mean(twos[[scenario]], na.rm=T),
                sd(twos[[scenario]], na.rm=T)))
}
