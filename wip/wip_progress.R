
#' @rdname progress
#' @title TODO
#' @description TODO
#' @param x [TODO] TODO
# @details TODO
# @importFrom TODO TODO
# @export
# @example inst/examples/example-progress.R
#' @examples
#' \dontrun{
#'
#' # TODO
#' }

#' @rdname progress
#' @name heading
#' @export
heading = function(...){
    dots = list(...)
    dots$col %<>% replace_null("cyan")
    dots %>% do.call(what = cli::cat_rule)
}

#' @rdname progress
#' @name bullet
#' @export
bullet = function(x = NULL, ...){
    dots = list(...)
    dots$x = str_c(" ", x)
    dots$col %<>% replace_null("grey75")
    dots %>% do.call(what = cli::cat_bullet)
}

#' @rdname progress
#' @name item
#' @export
item = function(x = NULL, ...){
    dots = list(...)
    dots$x = str_c(" ", x)
    dots$col %<>% replace_null("grey75")
    dots$bullet %<>% replace_null("line")
    dots %>% do.call(what = cli::cat_bullet)
}

#' @rdname progress
#' @name tick
#' @export
tick = function(x = NULL, ...){
    dots = list(...)
    dots$x = str_c(" ", x)
    dots$bullet = "tick"
    dots$col %<>% replace_null("grey75")
    dots$bullet_col %<>% replace_null("green")
    dots %>% do.call(what = cli::cat_bullet)
}

#' @rdname progress
#' @name item_timer
#' @export
item_timer = function(){

}

#' @rdname progress
#' @name item_tick
#' @export
item_tick = function(){

}


if(0){

    cli::cat_line("This is ", "a ", "line of text.", col = "red")
    cli::cat_bullet(letters[1:5])
    cli::cat_bullet(letters[1:5], bullet = "tick", bullet_col = "green")
    cli::cat_rule()

    heading("heading")
    bullet("bullet")
    item("item")
    tick("tick")

}

