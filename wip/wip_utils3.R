
any_na = function(x) any(is.na(x))

all_na = function(x) all(is.na(x))

sum_na = function(x) sum(is.na(x))

not_any_na = function(x) !any(is.na(x))

not_all_na = function(x) !all(is.na(x))

is_category = function(x) inherits(x, c("character", "factor"))

is_number = function(x) inherits(x, c("integer", "numeric"))

check_is_scalar_character = function(x){
    if(!is_scalar_character(x)){
        stop(substitute(x), " should be a character scalar")
    }
    invisible(x)
}

check_is_scalar_logical = function(x){
    if(!is_scalar_logical(x)){
        stop(substitute(x), " should be a logical scalar")
    }
    invisible(x)
}

check_is_scalar_numeric = function(x){
    if(!is_scalar_numeric(x)){
        stop(substitute(x), " should be a numeric scalar")
    }
    invisible(x)
}

check_is_scalar_integer = function(x){
    if(!is_scalar_integer(x)){
        stop(substitute(x), " should be a integer scalar")
    }
    invisible(x)
}
