
#' @rdname dttm-utils
#' @name dttm-utils
#' @title Date-time utility functions
#' @description
#' \itemize{
#'   \item \strong{\code{tz_get}} - extracts the timezone
#'   \item \strong{\code{tz_set}} - changes the timezone
#'   \item \strong{\code{tz_conv}} - changes the timezone and the clock time
#' }
#' These function are just wrappers for \link[lubridate]{tz}, \link[lubridate]{force_tz}
#' and \link[lubridate]{with_tz} but I find their names more intuitive and easier to
#' remember than the \pkg{lubridate} funtions.
#' @param x \[\link[base]{POSIXct} or \link[base]{POSIXlt}\] object
#' @param tz \[string\] the desired time zone (must be one of
#'   \link[base]{OlsonNames})
#' @return Returns a
#' @example inst/examples/examples-dttm-utils.R
NULL

#' @rdname  dttm-utils
#' @name tz_get
#' @export
tz_get = function(x){
    stopifnot(length(x) == 1 && inherits(x, c("POSIXct", "POSIXlt")))
    # attr(as.POSIXlt(x[1]), "tzone")[[1]]
    lubridate::tz(x)
}

#' @rdname  dttm-utils
#' @name tz_set
#' @export
tz_set = function(x, tz){
    stopifnot(length(x) == 1 && inherits(x, c("POSIXct", "POSIXlt")))
    stopifnot(length(tz) == 1 && tz %in% OlsonNames())
    lubridate::force_tz(x, tz)
}

#' @rdname  dttm-utils
#' @name tz_conv
#' @export
tz_conv = function(x, tz){
    stopifnot(length(x) == 1 && inherits(x, c("POSIXct", "POSIXlt")))
    # as.POSIXct(as.POSIXlt(x, tz = tz))
    lubridate::with_tz(x, tz)
}

# @title Convert unix time to datetime
# @description Wrapper for \link[base]{as.POSIXct.numeric} using \code{origin =
#   "1970-01-01"}. Assumes units are in seconds.
#
# \url{https://en.wikipedia.org/wiki/Unix_time}
#
# \url{https://www.epochconverter.com}
# @param x numeric vector
# @param tz time zone
# @export
# @examples
# \dontrun{
#
# convert_unix_time(0)
# convert_unix_time(1000000000)
# convert_unix_time(1e9)
#
# tz = "Europe/London"
# lubridate::now(tz) %>% as.numeric %>% convert_unix_time(tz)
# }
convert_unix_time = function(x, tz = "Europe/London"){
    as.POSIXct.numeric(x, tz = tz, origin = "1970-01-01")
}

