
#' @rdname pipe-friendly
#' @name not_na
#' @title Pipe-friendly functions
#' @description Series of shorthand functions to replace chains of \pkg{magrittr}
#'   functions.
#' @param x a numeric or character vector
#' @param y a numeric or character vector
#' @importFrom magrittr not
#' @importFrom magrittr is_in
#' @export
#' @examples
#' library(magrittr)
#'
#' x = c('H', 'e', 'l', 'l', 'o', '!', NA)
#' y = letters
#'
#' x %>% is.na # base R
#' x %>% not_na
#' x %>% which_na
#' x %>% which_not_na
#' x %>% all_na
#' x %>% all_not_na
#' x %>% any_na
#' x %>% any_not_na
#'
#' x %>% is_in(y) # magrittr
#' x %>% not_in(y)
#' x %>% which_in(y)
#' x %>% which_not_in(y)
#' x %>% all_in(y)
#' x %>% all_not_in(y)
#' x %>% any_in(y)
#' x %>% any_not_in(y)
not_na = function(x) x %>% is.na %>% not

#' @rdname pipe-friendly
#' @name na
#' @export
all_na = function(x) x %>% is.na %>% all

#' @rdname pipe-friendly
#' @name all_not_na
#' @export
all_not_na = function(x) x %>% is.na %>% not %>% all

#' @rdname pipe-friendly
#' @name any_na
#' @export
any_na = function(x) x %>% is.na %>% any

#' @rdname pipe-friendly
#' @name any_not_na
#' @export
any_not_na = function(x) x %>% is.na %>% not %>% any

#' @rdname pipe-friendly
#' @name which_na
#' @export
which_na = function(x) x %>% is.na %>% which

#' @rdname pipe-friendly
#' @name which_not_na
#' @export
which_not_na = function(x) x %>% is.na %>% not %>% which

#' @rdname pipe-friendly
#' @name all_in
#' @export
all_in = function(x, y) x %>% is_in(y) %>% all

#' @rdname pipe-friendly
#' @name all_not_in
#' @export
all_not_in = function(x, y) x %>% is_in(y) %>% not %>% all

#' @rdname pipe-friendly
#' @name not_na
#' @export
not_in = function(x, y) x %>% is_in(y) %>% not

#' @rdname pipe-friendly
#' @name any_in
#' @export
any_in = function(x, y) x %>% is_in(y) %>% any

#' @rdname pipe-friendly
#' @name any_not_in
#' @export
any_not_in = function(x, y) x %>% is_in(y) %>% not %>% any

#' @rdname pipe-friendly
#' @name which_in
#' @export
which_in = function(x, y) x %>% is_in(y) %>% which

#' @rdname pipe-friendly
#' @name which_not_in
#' @export
which_not_in = function(x, y) x %>% is_in(y) %>% not %>% which

