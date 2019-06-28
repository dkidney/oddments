
#' @rdname predicates
#' @title Predicate functions
#' @description All of these functions return a logical scalar.
#' @param x object to test
# @param y a numeric or character vector
#' @examples
#' \dontrun{
#' 
#' library(magrittr)
#'
#' x = c('H', 'e', 'l', 'l', 'o', '!', NA)
#' y = letters
#'
#' x %>% is.na # base R
#' x %>% not_missing
#' x %>% which_missing
#' x %>% which_not_missing
#' x %>% all_missing
#' x %>% none_missing
#' x %>% any_missing
#' x %>% any_not_missing
#'
#' x %>% is_in(y) # magrittr
#' x %>% not_in(y)
#' x %>% which_in(y)
#' x %>% which_not_in(y)
#' x %>% all_in(y)
#' x %>% none_in(y)
#' x %>% any_in(y)
#' x %>% any_not_in(y)
#' }
NULL

# scalars -----

#' @rdname predicates
#' @name is_scalar
#' @export
is_scalar <- function(x) {
    is.atomic(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_chr_scalar
#' @export
is_chr_scalar <- function(x) {
    is.character(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_dbl_scalar
#' @export
is_dbl_scalar <- function(x) {
    is.double(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_int_scalar
#' @export
is_int_scalar <- function(x) {
    rlang::is_integerish(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_lgl_scalar
#' @export
is_lgl_scalar <- function(x) {
    is.logical(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_num_scalar
#' @export
is_num_scalar <- function(x) {
    is.numeric(x) && length(x) == 1
}

#' @rdname predicates
#' @name is_positive_scalar
#' @export
is_positive_scalar <- function(x) {
    is_num_scalar(x) && x > 0
}

#' @rdname predicates
#' @name is_proportion_scalar
#' @export
is_proportion_scalar <- function(x) {
    is_num_scalar(x) && x > 0 && x < 1
}

#' @rdname predicates
#' @name is_true
#' @export
is_true = function(x){
    x %in% TRUE
}

#' @rdname predicates
#' @name is_false
#' @export
is_false = function(x){
    x %in% FALSE
}

#' @rdname predicates
#' @name is_not_true
#' @export
is_not_true = function(x){
    !x %in% TRUE
}

#' @rdname predicates
#' @name is_not_false
#' @export
is_not_false = function(x){
    !x %in% FALSE
}

# vectors -----

#' @rdname predicates
#' @name is_atomic_vector
#' @export
is_atomic_vector <- function(x) {
    is.atomic(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_dbl_vector
#' @export
is_dbl_vector <- function(x) {
    is.double(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_chr_vector
#' @export
is_chr_vector <- function(x) {
    is.character(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_lgl_vector
#' @export
is_lgl_vector <- function(x) {
    rlang::is_integerish(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_lgl_vector
#' @export
is_lgl_vector <- function(x) {
    is.logical(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_num_vector
#' @export
is_num_vector <- function(x) {
    is.numeric(x) && is.null(dim(x))
}

#' @rdname predicates
#' @name is_positive_vector
#' @export
is_positive_vector <- function(x) {
    is_num_vector(x) && all(x > 0)
}

#' @rdname predicates
#' @name is_proportion_vector
#' @export
is_proportion_vector <- function(x) {
    is_num_vector(x) && all(x <= 0) && all(x >= 1)
}

# sets -----

#' @rdname predicates
#' @name is_one_of
#' @export
is_one_of <- function(x, values) {
    length(x) == 1 && x %in% values
}

#' @rdname predicates
#' @name is_subset_of
#' @export
is_subset_of <- function(x, values) {
    all(x %in% values)
}

#' @rdname predicates
#' @name has_all
#' @export
has_all <- function(x, values, na.rm = TRUE) {
    all(values %in% x, na.rm = na.rm)
}

#' @rdname predicates
#' @name has_any
#' @export
has_any <- function(x, values, na.rm = TRUE) {
    any(values %in% x, na.rm = na.rm)
}

# missing -----

#' @rdname predicates
#' @name all_missing
#' @export
all_missing = function(x) {
    all(is.na(x))
}

#' @rdname predicates
#' @name any_missing
#' @export
any_missing = function(x) {
    any(is.na(x))
}

# misc -----

#' @rdname predicates
#' @name any_duplicated
#' @export
any_duplicated = function(x, na.rm = TRUE){
    any(duplicated(x), na.rm = na.rm)
}

#' @rdname predicates
#' @name all_finite
#' @export
all_finite = function(x) {
    all(is.finite(x))
}

#' @rdname predicates
#' @name is_square_matrix
#' @export
is_square_matrix = function(x){
    inherits(x, 'matrix') && nrow(x) == ncol(x)
}

