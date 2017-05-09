# Script to retrieve historical weather information for a nearby active station

library(jsonlite)
library(ncdf4)
library(yaml)

config <- yaml.load_file('weather.yml')

knmi_file = paste("output", "knmi.nc", sep="/")
if (!file.exists(knmi_file)) {
	download.file(config$url, knmi_file)
}


knmi <- list()
knmi$nc <- nc_open(knmi_file)
knmi$lat <- ncvar_get(knmi$nc, 'lat')
knmi$lon <- ncvar_get(knmi$nc, 'lon')
# Simple distance calculation (does not account for WGS coordinate geometry)
knmi$distance <- abs(config$lat - knmi$lat) + abs(config$lon - knmi$lon)

num_measurements <- knmi$nc$var$TG$size[1]

get_temperatures <- function(end_index) {
	latest_measurements <- ncvar_get(knmi$nc, 'TG',
									 start=c(end_index-1,1), count=c(1,-1))
	active_stations <- which(!is.na(latest_measurements))

	# Select closest active weather station.
	station <- which.min(knmi$distance[active_stations])

	ncvar_get(knmi$nc, 'TG', start=c(1,station), count=c(-1,1))
}

na_temperatures <- get_temperatures(num_measurements)
first_measurement <- which(!is.na(na_temperatures))[1]
seconds <- knmi$nc$dim$time$vals[first_measurement:num_measurements]
dates <- as.Date(as.POSIXct(seconds, origin=config$origin))
temperatures <- na_temperatures[first_measurement:num_measurements]
names(temperatures) <- dates

write(toJSON(as.list(temperatures), auto_unbox=T),
	  paste("output", "weather.json", sep="/"))