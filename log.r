# Utility file for setting up logging.

library(logging)

log_setup <- function() {
	args <- commandArgs(FALSE)
	log_arg <- match("--log", args, nomatch=0)
	if (log_arg > 0) {
		if (log_arg >= length(args)) {
			stop('--log requires a parameter')
		}
		level <- args[log_arg + 1]
	}
	else {
		level <- 'WARNING'
	}

	if (level %in% names(loglevels)) {
		logging::basicConfig(level)
	}
	else {
		stop(paste('--log must be one of',
				   paste(names(loglevels), collapse=", ")))
	}
}
log_setup()
