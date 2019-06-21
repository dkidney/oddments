
#' @rdname dttm-utils
#' @name dttm-utils
#' @title Date-time utility functions
#' @description
#' \itemize{
#'   \item \strong{\code{tz_get}} - extracts the timezone attribute from a date-time
#'   object and returns it as a string
#'   \item \strong{\code{tz_set}} - changes the timezone attribute of a date-time object,
#'   updates the time and returns a \link[base]{POSIXct} object
#' }
#' @param x \[POSIXct\] object - i.e. a date-time (see \link[base]{DateTimeClasses})
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
    attr(as.POSIXlt(x[1]), "tzone")[[1]]
}

#' @rdname  dttm-utils
#' @name tz_set
#' @export
tz_set = function(x, tz){
    stopifnot(length(x) == 1 && inherits(x, "POSIXct"))
    stopifnot(length(tz) == 1 && tz %in% OlsonNames())
    as.POSIXct(as.POSIXlt(x, tz = tz))
}

