# Script to retrieve historical weather information for a nearby active station

library(jsonlite)
library(ncdf4)
library(yaml)

source('include/args.r')
source('include/log.r')

config <- yaml.load_file('weather.yml')
output_directory <- get_arg('--output', default='output')

knmi_file = paste(output_directory, "knmi.nc", sep="/")
if (!file.exists(knmi_file)) {
	tryCatch(download.file(config$url, knmi_file, mode="wb"),
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
knmi$distance <- abs(config$lat - knmi$lat) + abs(config$lon - knmi$lon)

num_measurements <- knmi$nc$var$TG$size[1]

get_temperatures <- function(end_index) {
	active_stations <- logical(0)
	while (length(active_stations) == 0 && end_index > 1) {
		latest_measurements <- ncvar_get(knmi$nc, 'TG',
									 	 start=c(end_index-1,1), count=c(1,-1))
		active_stations <- which(!is.na(latest_measurements))
		end_index <- end_index - 1
	}

	if (length(active_stations) == 0) {
		stop("Could not find any active weather stations")
	}

	# Select closest active weather station.
	station <- active_stations[which.min(knmi$distance[active_stations])]

	ncvar_get(knmi$nc, 'TG', start=c(1,station), count=c(end_index,1))
}

na_temperatures <- get_temperatures(num_measurements)
first_measurement <- which(!is.na(na_temperatures))[1]
seconds <- knmi$nc$dim$time$vals[first_measurement:length(na_temperatures)]
dates <- as.Date(as.POSIXct(seconds, origin=config$origin))
temperatures <- na_temperatures[first_measurement:length(na_temperatures)]
names(temperatures) <- dates

write(toJSON(as.list(temperatures), auto_unbox=T),
	  paste(output_directory, "weather.json", sep="/"))
