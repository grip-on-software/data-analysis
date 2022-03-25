# Script to read BigBoat status information and create a plot

library(jsonlite)
library(ggplot2)

result <- data.frame()
handler <- function (df) {
    dc <- sum(as.numeric(df$ok))/length(df$ok)
    dt <- as.POSIXct(max(df$checked_time))
    result <<- rbind(result, list(count=dc, time=dt))
}

stream_in(file("data_status.json"), handler=handler, pagesize=1, verbose=F)
print(result)
plot <- ggplot(data=result,
               aes(x=as.POSIXct(result$time, origin="1970-01-01 00:00.00 UTC"),
                      y=result$count)) +
    geom_smooth() + ylim(0, 1) + scale_x_datetime(date_labels="%b %d") +
    labs(title="BigBoat reliability status", x="time", y="status")
ggsave('output/status.pdf')
print('Exported plot to output/status.pdf')
