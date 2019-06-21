\dontrun{

library(tidyverse)

# get the current local time and time zone
(x = Sys.time())
tz_get(x)

# get the time at the desired time zone
tz = "Asia/Kathmandu"
(y = tz_set(x, tz))
tz_get(y)

# comparison of different methods

tests = tibble(test = list(
    lubridate::`tz<-`(x, tz), # updates the time zone but not the time
    lubridate::force_tz(x, tz),  # updates the time zone but not the time
    as.POSIXct(x, tz = tz), # doen't update the time zone or the time
    as.POSIXlt(x, tz = tz), # updates the time zone and the time but changes the class
    as.POSIXct(format(x, tz = tz, usetz = TRUE), tz = tz), # inaccurately changes the time
    as.POSIXct(as.POSIXlt(x, tz = tz)), # does the job
    tz_set(x, tz) # this is just a wrapper for the previous method
))

tests %>%
    mutate(
        tz    = test %>% map_lgl(~isTRUE(try(identical(tz_get(.x), !!tz)))),
        class = test %>% map_lgl(~isTRUE(try(identical(class(.x), class(x))))),
        time  = test %>% map_lgl(~isTRUE(try(as.numeric(difftime(.x, x)) == 0)))
    )


}