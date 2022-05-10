# Script to retrieve historical weather data from a nearby active station.
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
library(ncdf4)
library(yaml)

source('include/args.r')
source('include/database.r')
source('include/log.r')

make_opt_parser(desc="Retrieve daily weather information for a nearby station",
                options=list(make_option('--output', default='output',
                                         help='Output directory')))
config <- get_config()
arguments <- config$args
weather <- config$weather
log_setup(arguments)

knmi_file <- paste(arguments$output, "knmi.nc", sep="/")
if (!file.exists(knmi_file)) {
    files <- url(weather$url, headers=list("Authorization"=weather$api_key))
    urls <- fromJSON(files)
    file <- url(paste(weather$url, urls$files[1]$filename, "url", sep="/"),
                headers=list("Authorization"=weather$api_key))
    data <- fromJSON(file)
    tryCatch(download.file(data$temporaryDownloadUrl, knmi_file, mode="wb"),
             error=function(e) {
                 logwarn(paste("Download problems: ", e))
                 quit("no", status=0, runLast=F)
             })
}


knmi <- list()
tryCatch(knmi$nc <- nc_open(knmi_file),
         error=function(e) {
             logwarn(paste("Could not open NetCDF file: ", e))
             quit("no", status=0, runLast=F)
         })

knmi$lat <- ncvar_get(knmi$nc, 'lat')
knmi$lon <- ncvar_get(knmi$nc, 'lon')
# Simple distance calculation (does not account for WGS coordinate geometry)
knmi$distance <- abs(weather$lat - knmi$lat) + abs(weather$lon - knmi$lon)

num_measurements <- knmi$nc$var$TG$size[1]

get_temperatures <- function(end_index) {
    active_stations <- logical(0)
    while (length(active_stations) == 0 && end_index > 1) {
        latest_measurements <- ncvar_get(knmi$nc, 'TG',
                                         start=c(end_index - 1, 1),
                                         count=c(1, -1))
        active_stations <- which(!is.na(latest_measurements))
        end_index <- end_index - 1
    }

    if (length(active_stations) == 0) {
        logwarn("Could not find any active weather stations")
        quit("no", status=0, runLast=F)
    }

    # Select closest active weather station.
    station <- active_stations[which.min(knmi$distance[active_stations])]

    ncvar_get(knmi$nc, 'TG', start=c(1, station), count=c(end_index, 1))
}

na_temperatures <- get_temperatures(num_measurements)
first_measurement <- which(!is.na(na_temperatures))[1]
seconds <- knmi$nc$dim$time$vals[first_measurement:length(na_temperatures)]
dates <- as.Date(as.POSIXct(seconds, origin=weather$origin))
temperatures <- na_temperatures[first_measurement:length(na_temperatures)]
names(temperatures) <- dates

write(toJSON(as.list(temperatures), auto_unbox=T),
      paste(arguments$output, "weather.json", sep="/"))
