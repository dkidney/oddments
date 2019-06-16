
formulas = function(D = ~1, g0 = 1, sigma = ~1, z = ~1, bearings = ~1, distances = ~1, pcall = ~1){

    args = as.list(environment())

    # change NULL to intercept-only
    args %<>% map_if(is_null, ~formula(~ 1))

    # coerce characters to formula
    args %<>% map_if(is_scalar_character, function(x){
        if(!str_detect(x, "^~")) x %<>% str_c("~", .)
        as.formula(x)
    })

    # check for invalid classes
    errors = args %>%
        map_lgl(~ is_formula(.x) || is_scalar_numeric(.x)) %>%
        not
    if(any(errors)){
        stop("invalid input(s) for: ", names(args)[errors] %>% str_c(collapse = ", "))
    }

    # update left hand side of formulas?
    args %<>% map2(names(args), function(form, par){
        if(is_formula(form)){
            lhs = form %>% form_response
            if(is.null(lhs)){
                form %<>% update(str_c(par, "~ ."))
            }
        }
        form
    })

    # return summary data frame
    args %>% {
        data_frame(
            par = names(.),
            fixed = map_dbl(., function(x){
                if(is_formula(x)) NA_real_ else x
            }),
            formula = map_chr(., function(x){
                if(is_formula(x)){
                    x %>% form_response_remove %>% form_as_character
                }else{
                    NA_character_
                }
            })
        )
    }
}

if(0){
    # debugonce(formulas)
    formulas()
    formulas(D = 1)
    formulas(D = "~ 1")
    formulas(D = "g")
    formulas(D = NULL)
    formulas(D = FALSE)
    formulas(D = 1:10)
    formulas(D = c("~f", "~g"))
}








