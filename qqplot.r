library(qqplotr)

qqplot_monte_carlo <- function(counts) {
    if (!qqplot) {
        loginfo("No qqplotr library installed, skipping qqplot generation")
        return
    }
    df <- data.frame(sample=colMeans(counts, na.rm=T),
                     ymin=sapply(counts, min, na.rm=T),
                     ymax=sapply(counts, max, na.rm=T))
    ggplot(data=df, aes(sample=df$sample)) +
        stat_qq_line() +
        stat_qq_point() +
        labs(x="Theoretical Quantiles", y="Sample Quantiles")
    ggsave("qqplot_monte_carlo.pdf")
}

# TODO: Read json
