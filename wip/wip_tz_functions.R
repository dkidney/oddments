
# http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# OlsonNames()

tz_set = function(x, tz){
    stopifnot(inherits(x, "POSIXct") && length(x) == 1)
    as.POSIXct(format(x, tz = tz, usetz = TRUE), tz = tz)
}

tz_get = function(x){
    stopifnot(inherits(x, "POSIXct") && length(x) == 1)
    lubridate::tz(x)
}


if(0){

    # lubridate::tz changes tz but not the time
    x = y = Sys.time()
    lubridate::tz(y) = "America/New_York"
    tz_get(x)
    tz_get(y)
    x
    y

    # lubridate::force_tz changes tz but not the time
    x = y = Sys.time()
    y = lubridate::force_tz(y, "America/New_York")
    tz_get(x)
    tz_get(y)
    x
    y

    # set_tz changes tz and time
    x = y = Sys.time()
    y = tz_set(x, "America/New_York")
    tz_get(x)
    tz_get(y)
    x
    y

}