# Utility file to extract command line arguments

if (!exists('INC_ARGS_R')) {
    INC_ARGS_R <- T

    args <- commandArgs(FALSE)

    has_arg <- function(name) {
        return(name %in% args)
    }
    get_arg <- function(name, default=NA) {
        arg <- match(name, args, nomatch=0)
        if (arg > 0) {
            if (!is.na(default) && default == F) {
                return(T)
            }
            else if (arg >= length(args)) {
                stop(paste(name, 'requires a parameter'))
            }
            return(args[arg + 1])
        }
        else {
            return(default)
        }
    }
}
