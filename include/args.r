# Utility file to extract command line arguments.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

if (!exists('INC_ARGS_R')) {
    INC_ARGS_R <- T

    library(optparse)

    parser <- NULL

    make_opt_parser <- function(desc="", options=NULL, variables=NULL) {
        index <- 1
        for (option in options) {
            if (option@action == "store_true" &&
                !startsWith(option@long_flag, "--no-")) {
                new_option <- make_option(sub("^--", "--no-", option@long_flag),
                                          action="store_false",
                                          type=option@type,
                                          dest=option@dest,
                                          default=option@default,
                                          help="Invert former option")
                options <- append(options, list(new_option), index)
                index <- index + 1
            }
            else if (option@action == "store") {
                option@help <- paste(option@help, "(default: %default)")
                options[[index]] <- option
            }
            index <- index + 1
        }
        parser <<- OptionParser(option_list=options, description=desc)

        if (exists('INC_LOG_R')) {
            parser <<- add_option(parser, c("-l", "--log"), default='WARNING',
                                  metavar="LEVEL",
                                  help=paste("Log level, one of",
                                             paste(names(loglevels),
                                                   collapse=", "),
                                             "(default: %default)"))
        }
        if (exists('INC_DATABASE_R')) {
            parser <<- add_option(parser, "--config", default='config.yml',
                                  help="Configuration file (default: %default)")
            parser <<- add_option(parser, "--org",
                                  default=Sys.getenv("ANALYSIS_ORGANIZATION"),
                                  help="Organization name (default: %default)")
        }

        for (name in names(variables)) {
            if (is.list(variables[[name]])) {
                var <- list(default=variables[[name]]$field,
                            metavar=name,
                            help=paste(variables[[name]]$description,
                                       "(default: %default)"))
            }
            else {
                if (startsWith(variables[[name]], '$')) {
                    var <- list(default=NULL,
                                metavar=substring(variables[[name]], 2))
                }
                else {
                    var <- list(default=variables[[name]], metavar=name)
                }
            }
            flag <- paste('--', gsub('_', '-', name), sep='')
            parser <<- do.call(add_option, c(parser, flag, var))
        }
    }
    get_opt_args <- function(args=NULL, partial=FALSE) {
        if (partial) {
            invalid <- T
            while (invalid) {
                ref <- try(parse_args(parser, args=args,
                                      convert_hyphens_to_underscores=TRUE),
                           silent=T)
                if (invalid <- (class(ref) == "try-error")) {
                    m <- regmatches(ref[1],
                                    regexec('(\\w+) flag "(.+)" is invalid',
                                       ref[1], perl=T))[[1]]
                    if (length(m) > 0) {
                        flag <- paste(ifelse(m[2] == 'long', '--', '-'), m[3],
                                      sep='')
                        arg <- match(flag, args, nomatch=0)
                        if (arg > 0) {
                            args <- args[-arg]
                            if (length(args) >= arg &&
                                !startsWith(args[arg], "-")) {
                                args <- args[-arg]
                            }
                            next
                        }
                    }

                    print_help(parser)
                    stop(ref[1])
                }
            }
        }
        return(tryCatch(parse_args(parser, args=c(commandArgs(TRUE), args),
                                   positional_arguments=FALSE,
                                   convert_hyphens_to_underscores=TRUE),
                        error=function(ref) {
                            print_help(parser)
                            stop(ref)
                        }))
    }
}
