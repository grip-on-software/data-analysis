# Utility file for setting up logging.
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
