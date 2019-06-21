
# http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# OlsonNames()

tz_set = function(x, tz){
    stopifnot(inherits(x, c("POSIXct", "POSIXlt")))
    stopifnot(length(tz) == 1 && tz %in% OlsonNames())
    as.POSIXct(as.POSIXlt(x, tz = tz))
}

tz_get = function(x){
    stopifnot(inherits(x, c("POSIXct", "POSIXlt")))
    lubridate::tz(x)
}


if(0){
    
    library(tidyverse)
    
    tz = "America/New_York"
    (x = Sys.time())
    
    tests = tibble(test = list(
        lubridate::`tz<-`(x, tz),
        lubridate::force_tz(x, tz),
        as.POSIXlt(x, tz = tz),
        as.POSIXct(x, tz = tz),
        as.POSIXct(format(x, tz = tz, usetz = TRUE), tz = tz),
        as.POSIXct(as.POSIXlt(x, tz = tz)),
        tz_set(x, tz)
    ))
    
    tests %>% 
        mutate(
            tz    = test %>% map_lgl(~identical(tz_get(.x), !!tz)),
            class = test %>% map_lgl(~identical(class(.x), class(x))),
            time  = test %>% map_lgl(~as.numeric(difftime(.x, x)) == 0)
        )
    
    # lubridate::tz and lubridate::force_tz change tz but not the time
    # as.POSIXlt changes the class
    # as.POSIXct doesn't do anything...
    # as.POSIXlt(format(...)) doesn't preserve the time accurately
    # as.POSIXct(as.POSIXlt(...)) does the job
    # set_tz is just a wrapper for as.POSIXct(as.POSIXlt(...)) 
    
}
