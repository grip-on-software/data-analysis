# Script to make a QQ plot diagram for tabular data from res.r 

library(ggplot2)
if ("qqplotr" %in% rownames(installed.packages())) {
    library(qqplotr)
    QQPLOTR <- T
} else {
    QQPLOTR <- F
}

qqplot_monte_carlo <- function(counts, file) {
    if (!QQPLOTR) {
        return
    }
    df <- data.frame(sample=colMeans(counts, na.rm=T),
                     ymin=sapply(counts, min, na.rm=T),
                     ymax=sapply(counts, max, na.rm=T))
    ggplot(data=df, aes(sample=df$sample)) +
        stat_qq_line(colour="#3366FF") +
        stat_qq_point() +
        labs(x="Theoretical Quantiles", y="Sample Quantiles")
    ggsave(file)
}

count <- 10000
for (file in Sys.glob("qqplot.*.txt")) {
    counts <- matrix(as.numeric(unlist(read.table(file))), ncol=count, byrow=T)
    qqplot_monte_carlo(counts, sub("\\.txt", ".pdf", file))
}
