# Script to read BigBoat status information and create a plot.
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

library(jsonlite)
library(ggplot2)

result <- data.frame()
handler <- function(df) {
    dc <- sum(as.numeric(df$ok))/length(df$ok)
    dt <- as.POSIXct(max(df$checked_time))
    result <<- rbind(result, list(count=dc, time=dt))
}

stream_in(file("data_status.json"), handler=handler, pagesize=1, verbose=F)
print(result)
plot <- ggplot(data=result,
               aes(x=as.POSIXct(result$time, origin="1970-01-01 00:00.00 UTC"),
                      y=result$count)) +
    geom_smooth() + ylim(0, 1) + scale_x_datetime(date_labels="%b %d") +
    labs(title="BigBoat reliability status", x="time", y="status")
ggsave('output/status.pdf')
print('Exported plot to output/status.pdf')
