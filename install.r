# Script to install all requirements.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
# Copyright 2017-2023 Leon Helwerda
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

install.packages("devtools")

if (!require(devtools)) {
    print('Could not install or find devtools')
    quit("no", status=1, runLast=F)
}

requirements_file <- Sys.getenv("ANALYSIS_REQUIREMENTS",
                                unset='requirements.txt')
expected <- c()
for (line in readLines(requirements_file)) {
    if (endsWith(line, ";optional")) {
        print(paste("Not installing", line))
        next
    }
    versioned <- strsplit(line, "==")[[1]]
    if (length(versioned) == 1) {
        install.packages(line)
        expected <- c(expected, line)
    } else {
        install_version(versioned[1], version=versioned[2])
        expected <- c(expected, versioned[1])
    }
}
ok <- expected %in% rownames(installed.packages())
if (!all(ok)) {
    print(paste('Could not install some packages:',
                paste(expected[!ok], collapse=', ')))
    quit("no", status=1, runLast=F)
}
