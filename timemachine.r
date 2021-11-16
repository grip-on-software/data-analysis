library(jsonlite)
library(ggplot2)

min <- 110
step <- 10
# jq -c 'map(2 * (.metrics.precision * .metrics.recall) / (.metrics.precision + .metrics.recall))' sprint_labels.json > time-machine.json
scores <- as.numeric(read_json('time-machine.json'))
# grep -B1 "Current time is" time-machine-consoleText | grep -Po "Train: \d+" | cut -d' ' -f2 > time-machine-sizes
sizes <- as.numeric(readLines('time-machine-sizes'))
data <- data.frame(y=head(scores, -2), x=head(sizes, -2))

ggplot(data, aes(x=data$x, y=data$y)) +
    geom_point() +
    geom_line() +
    scale_x_continuous("Training set size") +
    scale_y_continuous("F1 score")
ggsave("time-machine.pdf", width=10, height=6)
