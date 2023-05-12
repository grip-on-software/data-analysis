# Script to make a train size/score plot based on a time machine prediction run.
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

# Data can be collected from a Jenkins CI build: the sprint_labels.json artifact
# and the console plain text should be first preprocessed using the commands:

# jq -c 'map(2 * (.metrics.precision * .metrics.recall) / .metrics.precision +
#     .metrics.recall))' sprint_labels.json > time-machine.json
# grep -B1 "Current time is" time-machine-consoleText | grep -Po "Train: \d+" |\
#     cut -d' ' -f2 > time-machine-sizes

library(jsonlite)
library(ggplot2)

min <- 110
step <- 10
scores <- as.numeric(read_json('time-machine.json'))
sizes <- as.numeric(readLines('time-machine-sizes'))
data <- data.frame(y=head(scores, -2), x=head(sizes, -2))

ggplot(data, aes(x=data$x, y=data$y)) +
    geom_point() +
    geom_line() +
    scale_x_continuous("Training set size") +
    scale_y_continuous("F1 score")
ggsave("time-machine.pdf", width=10, height=6)
