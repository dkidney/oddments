
# is_* -----

is_chr_scalar <- function(x) {
    is.character(x) && length(x) == 1
}

is_chr_vector <- function(x) {
    is.character(x) && is.null(dim(x))
}

is_int_scalar <- function(x) {
    rlang::is_integerish(x) && length(x) == 1
}

is_int_vector <- function(x) {
    rlang::is_integerish(x) && is.null(dim(x))
}

# is_intish -----

is_lgl_scalar <- function(x) {
    is.logical(x) && length(x) == 1
}

is_lgl_vector <- function(x) {
    is.logical(x) && is.null(dim(x))
}

is_num_scalar <- function(x) {
    is.numeric(x) && length(x) == 1
}

is_num_vector <- function(x) {
    is.numeric(x) && is.null(dim(x))
}

is_positive_scalar <- function(x) {
    is_num_scalar(x) && x > 0
}

is_positive_vector <- function(x) {
    is_num_vector(x) && all(x > 0)
}

is_proportion_scalar <- function(x) {
    is_num_scalar(x) && x > 0 && x < 1
}

is_proportion_vector <- function(x) {
    is_num_vector(x) && all(x <= 0) && all(x >= 1)
}

# is_dataframe -----

# is_macbook -----
# is_macbook = function(){
#     get_hostname() %>% str_detect("MacBook")
# }

# is_true -----
# is_true = function(x){
#     x %in% TRUE
# }

# is_false -----
# is_false = function(x){
#     x %in% FALSE
# }

# is_not_true -----
# is_not_true = function(x){
#     !x %in% TRUE
# }

# is_not_false -----
# is_not_false = function(x){
#     !x %in% FALSE
# }

is_one_of <- function(x, values) {
    length(x) == 1 && x %in% values
}

# has_* -----

contains_all <- function(x, values) {
    all(x %in% values)
}

# check_* -----

check_is_chr_scalar <- function(x) {
    if (!is_chr_scalar(x)) stop("expecting a character scalar", call. = FALSE)
}

check_is_chr_vector <- function(x) {
    if (!is_chr_vector(x)) stop("expecting character values", call. = FALSE)
}

check_is_int_scalar <- function(x) {
    if (!is_int_scalar(x)) {
        stop("expecting an integer scalar", call. = FALSE)
    }
}

check_is_int_vector <- function(x) {
    if (!is_int_vector(x)) {
        stop("expecting integer values", call. = FALSE)
    }
}

check_is_lgl_scalar <- function(x) {
    if (!is_lgl_scalar(x)) {
        stop("expecting a logical scalar", call. = FALSE)
    }
}

check_is_lgl_vector <- function(x) {
    if (!is_lgl_vector(x)) {
        stop("expecting logical values", call. = FALSE)
    }
}

check_is_num_scalar <- function(x) {
    if (!is_num_scalar(x)) {
        stop("expecting a numeric scalar", call. = FALSE)
    }
}

check_is_num_vector <- function(x) {
    if (!is_num_vector(x)) {
        stop("expecting numeric values", call. = FALSE)
    }
}

check_is_positive_scalar <- function(x) {
    if (!is_positive_scalar(x)) {
        stop("expecting a positive number", call. = FALSE)
    }
}

check_is_positive_vector <- function(x) {
    if (!is_positive_vector(x)) {
        stop("expecting positive values", call. = FALSE)
    }
}

check_is_proportion_scalar <- function(x) {
    if (!is_proportion_scalar(x)) {
        stop("expecting a proportion", call. = FALSE)
    }
}

check_is_proportion_vector <- function(x) {
    if (!is_proportion_vector(x)) {
        stop("expecting proportions", call. = FALSE)
    }
}

check_is_one_of <- function(x, values) {
    if (!length(x) == 1 || !is_one_of(x, values)) {
        stop("expecting one of ", collapse_values(values), call. = FALSE)
    }
}

check_contains_all <- function(x, values) {
    if (!contains_all(x, values)) {
        stop("expecting values to contain ", collapse_values(values), call. = FALSE)
    }
}

check_is_identical = function(x, y){
    if (!identical(x, y)) {
        stop("expecting ", y, call. = FALSE)
    }
}

# check_dir_exists -----

# check_file_exists -----
# check_file_exists = function(path){
#     expanded = path %>%
#         check_is_scalar_character %>%
#         path.expand
#     if(!file.exists(expanded)) stop("can't find ", expanded)
#     invisible(path)
# }

# helpers -----

collapse_values <- function(x) {
    if (is_chr_vector(x)) {
        paste0("'", paste0(x, collapse = "', '"), "'")
    } else {
        paste0(x, collapse = ", ")
    }
}

cat_values <- function(x) {
    x_chr <- x
    if (is.character(x)) {
        x_chr %<>%
            str_c("'", ., "'")
    }
    if (length(x) > 1) {
        x_chr %<>%
            str_c(collapse = ", ") %>%
            str_c("c(", ., ")")
    }
    x_chr
}

