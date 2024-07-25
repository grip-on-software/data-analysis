DROP AGGREGATE secondmax(INTEGER);
CREATE AGGREGATE secondmax(val INTEGER) RETURNS INTEGER LANGUAGE R {
	smax <- function(vals) {sort(vals,partial=length(vals)-1)[length(vals)-1]}
	if (!exists("aggr_group")) {
		as.vector(smax(val))
	}
	else {
		aggregate(val, by=list(aggr_group), FUN=smax)$x
	}
};

DROP AGGREGATE secondmax(TIMESTAMP);
CREATE AGGREGATE secondmax(val TIMESTAMP) RETURNS TEXT LANGUAGE R {
	smax <- function(vals) {sort(vals,partial=length(vals)-1)[length(vals)-1]}
	as.character(as.POSIXct(as.numeric(aggregate(val, by=list(aggr_group), FUN=smax)$x), origin="1970-01-01"))
};

DROP AGGREGATE secondmax_timestamp(TEXT);
CREATE AGGREGATE secondmax_timestamp(val TEXT) RETURNS TEXT LANGUAGE R {
	smax <- function(vals) {sort(vals,partial=length(vals)-1)[length(vals)-1]}
	as.character(aggregate(val, by=list(aggr_group), FUN=smax)$x)
};

DROP FUNCTION timestamp_to_double(TIMESTAMP);
CREATE FUNCTION timestamp_to_double(val TIMESTAMP) RETURNS DOUBLE LANGUAGE R {
	as.numeric(val)
};