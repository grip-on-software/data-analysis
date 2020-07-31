# Utility file to extract command line arguments

if (!exists('INC_ARGS_R')) {
    INC_ARGS_R <- T

    args <- commandArgs(FALSE)

    add_args <- function(new_args) {
        args <<- paste(args, new_args)
    }
    has_arg <- function(name) {
        return(name %in% args)
    }
    get_arg <- function(name, default=NA) {
        arg <- match(name, args, nomatch=0)
        if (arg > 0) {
            if (!is.na(default) && identical(default, F)) {
                return(T)
            }
            else if (arg >= length(args)) {
                stop(paste(name, 'requires a parameter'))
            }
            if (is.numeric(default)) {
                return(as.numeric(args[arg + 1]))
            }
            return(args[arg + 1])
        }
        else {
            if (isTRUE(default)) {
                return(!get_arg(sub("^--", "--no-", name), default=F))
            }
            return(default)
        }
    }
}
