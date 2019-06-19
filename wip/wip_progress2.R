
print_progress = function(...,
                          .colour = "white",
                          .bold = FALSE,
                          .new_line = TRUE,
                          .prefix = NULL,
                          .verbose = getOption("oddments_verbose")){
    txt = list(...) %>%
        map(as.character) %>%
        map_chr(str_flatten) %>%
        str_flatten %>%
        str_c(.prefix, .)
    if(!is.null(.colour) && !is.na(.colour)){
        txt %<>% crayon::make_style(.colour)()
    }
    if(.bold){
        txt %<>% crayon::bold()
    }
    if(.new_line){
        txt %<>% str_c("\n")
    }
    if(is.null(.verbose) || .verbose){
        cat(txt)
    }
    invisible(txt)
}

#' @rdname print_progress
#' @aliases print_progress
#' @name heading
#' @title Progress messages
#' @description Convenience functions for printing progress messsages to the console.
#' @param ... one or more character vectors
#' @param .width line width
#' @param .char character to use for the line or bullet
#' @param .colour text colour
#' @param .bold text weight (logical)
#' @param .prefix ???
#' @param .new_line if TRUE, a new line character \code{"\n"} will be added to the end of the text
#' @param .verbose if FALSE, messages will not be printed
#' @export
#' @examples
#' \dontrun{
#'
#' heading("a heading")
#' bullet("a bullet")
#' bullet("a ", "bullet")
#' bullet(c("a ", "bullet"))
#' bullets("some", "bullets")
#' bullets(c("some", "more", "bullets"))
#' item("an item")
#' items("some", "items")
#' items(c("some", "more", "items"))
#' tick()
#' error("an error")
#' warn("a warning")
#' }
heading = function(...,
                   .width = getOption("width"),
                   .char = getOption("oddments_heading_char"),
                   .colour = getOption("oddments_heading_col"),
                   .bold = FALSE,
                   .new_line = TRUE,
                   .verbose = getOption("oddments_verbose")){

    txt = str_flatten(c(...))
    if(length(txt) == 0) txt = "" else txt = str_c(txt, " ")
    txt %<>%
        str_pad(width = .width, side = "right", pad = .char) %>%
        str_c("\n", .)
    print_progress(
        txt,
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .verbose = .verbose
    )
}

#' @rdname print_progress
#' @name bullet
#' @export
bullet = function(...,
                  .char = getOption("oddments_bullet_char"),
                  .colour = getOption("oddments_bullet_col"),
                  .bold = FALSE,
                  .new_line = TRUE,
                  .verbose = getOption("oddments_verbose")){

    print_progress(
        ...,
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .prefix = str_c(.char, " "),
        .verbose = .verbose
    )
}

#' @rdname print_progress
#' @name item
#' @export
item = function(...,
                .char = getOption("oddments_item_char"),
                .colour = getOption("oddments_item_col"),
                .bold = FALSE,
                .new_line = TRUE,
                .verbose = getOption("oddments_verbose")){

    print_progress(
        ...,
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .prefix = str_c(" ", .char, " "),
        .verbose = .verbose
    )
}

#' @rdname print_progress
#' @name bullet
#' @export
bullets = function(...,
                   .char = getOption("oddments_bullet_char"),
                   .colour = getOption("oddments_bullet_col"),
                   .bold = FALSE,
                   .verbose = getOption("oddments_verbose")){

    c(...) %>%
        as.list %>%
        map(bullet, .char = .char, .colour = .colour, .bold = .bold, .verbose = .verbose)
    invisible()
}

#' @rdname print_progress
#' @name items
#' @export
items = function(...,
                 .char = getOption("oddments_item_char"),
                 .colour = getOption("oddments_item_col"),
                 .bold = FALSE,
                 .verbose = getOption("oddments_verbose")){

    c(...) %>%
        as.list %>%
        map(item, .char = .char, .colour = .colour, .bold = .bold, .verbose = .verbose)
    invisible()
}

#' @rdname print_progress
#' @name tick
#' @export
tick = function(.new_line = TRUE,
                .colour = getOption("oddments_heading_col"),
                .bold = FALSE,
                .verbose = getOption("oddments_verbose")){
    print_progress(
        "\u2714",
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .verbose = .verbose
    )
}

#' @rdname print_progress
#' @name warn
#' @export
warn = function(...,
                .prefix = TRUE,
                .colour = getOption("oddments_warn_col"),
                .bold = FALSE,
                .new_line = TRUE,
                .verbose = getOption("warn") >= 0){
    print_progress(
        ...,
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .prefix = if(.prefix) "WARNING: ",
        .verbose = .verbose
    )
}

#' @rdname print_progress
#' @name error
#' @export
error = function(...,
                 .prefix = TRUE,
                 .colour = getOption("oddments_error_col"),
                 .bold = FALSE,
                 .new_line = TRUE){
    print_progress(
        ...,
        .colour = .colour,
        .bold = .bold,
        .new_line = .new_line,
        .prefix = if(.prefix) "ERROR: ",
        .verbose = TRUE
    )
}
