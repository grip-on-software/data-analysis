# Utility file to extract command line arguments

if (!exists('INC_ARGS_R')) {
    INC_ARGS_R <- T

    args <- rev(commandArgs(FALSE))

    add_args <- function(new_args) {
        args <<- c(rev(new_args), "--", args)
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
            else if (arg == 1 || args[arg - 1] == "--") {
                stop(paste(name, 'requires a parameter'))
            }
            if (is.numeric(default)) {
                return(as.numeric(args[arg - 1]))
            }
            return(args[arg - 1])
        }
        else {
            if (isTRUE(default)) {
                return(!get_arg(sub("^--", "--no-", name), default=F))
            }
            return(default)
        }
    }
}
