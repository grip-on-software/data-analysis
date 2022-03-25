# Utility file for setting up logging.

if (!exists('INC_LOG_R')) {
    INC_LOG_R <- T

    library(logging)
    source('include/args.r')

    log_format <- function(obj) {
        # Print objects with classes that can be printed but are not formatted
        # correctly when using the object itself or other functions on it.
        return(paste(capture.output(print(obj)), collapse="\n"))
    }

    log_setup <- function(level='WARNING') {
        if (is.list(level)) {
            level <- level$log
        }

        if (level %in% names(loglevels)) {
            logging::basicConfig(level)
        }
        else {
            if (!is.null(parser)) {
                print_help(parser)
            }
            stop(paste('--log must be one of',
                       paste(names(loglevels), collapse=", "), 'not', level))
        }
    }
}
