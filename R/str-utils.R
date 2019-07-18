#' @rdname str-utils
#' @name str-utils
#' @title String utils
#' @description
#' \itemize{
#'     \item \code{collapse_str()} - collapses the input into a single string, character
#'     inputs are 'quoted' by default
#'     \item \code{commas()} - is a wrapper for \code{collapse_str()} using \code{.sep =
#'     ", "}
#'     \item \code{round_str()} - converts numeric/integer vector input into a character
#'     vector using a given number of decimal places (it wraps \link[base]{sprintf})
#'     \item \code{integer_str()} - converts numeric/integer vector input to integer
#'     format and returns a character vector without using scientific notation
#'     \item \code{difftime_str()} - converts a unit length \link[base]{difftime} object
#'     into a string that resembles the output of \link[base]{print.difftime}
#' }
#' @param ... one or more scalars and/or vectors of items to be coerced to strings
#' @param .sep TODO
#' @param .use_quotes indicates whether elements should be quoted (if \code{NULL} and if
#'   the \code{...} elements are character then quotes are used by default)
#' @param .width passed to \link[stringr]{str_wrap} as the \code{width} argument
#' @param .indent passed to \link[stringr]{str_wrap} as the \code{indent} argument
#' @param .exdent passed to \link[stringr]{str_wrap} as the \code{exdent} argument
#' @importFrom stringr str_c str_wrap
NULL

#' @rdname str-utils
#' @name collapse_str
#' @export
#' @examples
#' cat(collapse_str(0:100))
#' cat(collapse_str(0:100, .width = 20, .indent = 5, .exdent = 5))
#'
#' cat(collapse_str(letters, LETTERS))
#' cat(collapse_str(letters, LETTERS, .width = 20, .indent = 5, .exdent = 5))
#'
collapse_str <- function(...,
                         .sep = " ",
                         .use_quotes = NULL,
                         .width = NULL,
                         .indent = 0,
                         .exdent = 0) {
  x <- list(...) %>% unlist(FALSE, FALSE)
  stopifnot(is.null(dim(x)))
  if (length(x) == 0) {
    return("")
  }
  if (is.null(.use_quotes)) {
    .use_quotes <- is.character(x)
  }
  if (.use_quotes) {
    x %<>% str_c("'", ., "'")
  }
  x %<>% str_c(collapse = .sep)
  if (!is.null(.width)) {
    stopifnot(rlang::is_integerish(.width))
    stopifnot(length(.width) == 1)
    x %<>% str_wrap(width = .width, indent = .indent, exdent = .exdent)
  }
  x
}

#' @rdname str-utils
#' @name commas
#' @export
#' @examples
#' cat(commas(0:100))
#' cat(commas(letters, LETTERS))
#'
commas <- function(...,
                   .use_quotes = NULL,
                   .width = getOption("width"),
                   .indent = 0,
                   .exdent = 0) {
  collapse_str(
    ...,
    .sep = ", ",
    .use_quotes = .use_quotes,
    .width = .width,
    .indent = .indent,
    .exdent = .exdent
  )
}

#' @rdname str-utils
#' @name lorum
#' @export
#' @examples
#' cat(collapse_str(lorum))
#' cat(commas(lorum, .width = 20, .indent = 5, .exdent = 5))
#'
lorum <- c("Lorem", "ipsum", "dolor", "sit", "amet",
"consectetur", "adipiscing", "elit", "sed", "do",
"eiusmod", "tempor", "incididunt","ut", "labore",
"et", "dolore", "magna", "aliqua")

#' @rdname str-utils
#' @name round_str
#' @param x a numeric vector
#' @param digits [integer] number of decimal places
#' @export
#' @examples
#' 1 %>% round(3)
#' 1 %>% sprintf("%.3f", .)
#' 1 %>% round_str(3)
#'
#' pi %>% round(3)
#' pi %>% sprintf("%.3f", .)
#' pi %>% round_str(3)
#'
round_str <- function(x, digits = getOption("digits")) {
  stopifnot(is.numeric(x))
  stopifnot(is.null(dim(x)))
  out <- rep(NA_character_, length(x))
  i <- !is.na(x)
  if (!any(i)) {
    return(out)
  }
  out[i] <- sprintf(fmt = str_c("%.", digits, "f"), x[i])
  out
}

#' @rdname str-utils
#' @name integer_str
#' @export
#' @examples
#' 1e10 %>% integer_str()
#'
integer_str <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(is.null(dim(x)))
  x %>%
    as.numeric() %>%
    floor() %>%
    format(scientific = FALSE, trim = TRUE)
}

#' @rdname str-utils
#' @name difftime_str
#' @export
#' @param units [string] time units (one of "auto", "secs", "mins", "hours", "days",
#'   "weeks"))
#' @examples
#' start <- Sys.time()
#' stop <- Sys.time()
#' dt <- difftime(stop, start)
#' dt %>% print()
#' dt %>% format(units = "mins") # doesn't change units
#' dt %>% as.character()
#' dt %>% difftime_str()
#' dt %>% difftime_str(units = "secs")
#' dt %>% difftime_str(units = "mins")
#'
difftime_str <- function(x, digits = 1, units = "auto") {
  stopifnot(inherits(x, "difftime"))
  stopifnot(length(x) == 1)
  units <- match.arg(units, c("auto", "secs", "mins", "hours", "days", "weeks"))
  if (units == "auto") {
    units <- attr(x, "units")
  }
  paste(
    "Time difference of",
    round_str(as.numeric(x, units = units), digits = digits),
    units
  )
}
