#' @rdname progress
#' @name progress
#' @title Progress messages
#' @description TODO
#' @param ... arguments that get passed to \link[cli]{cat_bullet} (or
#'   \link[cli]{cat_rule} for \code{heading})
# @details TODO
#' @importFrom stringr str_length
#' @importFrom stringr str_pad
#' @importFrom stringr str_remove
#' @example inst/examples/examples-progress.R
NULL

#' @rdname progress
#' @name heading
#' @export
heading <- function(...) {
    cli::cat_rule(collapse_chr(...), col = "cyan")
}

#' @rdname progress
#' @name bullet
#' @export
bullet <- function(...) {
    info(collapse_chr(...), type = "bullet")
}

#' @rdname progress
#' @name item
#' @export
item <- function(...) {
    info(collapse_chr(...), type = "item")
}

#' @rdname progress
#' @name success
#' @export
success <- function(...) {
    info(collapse_chr(...), type = "success")
}

#' @rdname progress
#' @name concern
#' @export
concern <- function(...) {
    info(collapse_chr(...), type = "concern")
}

#' @rdname progress
#' @name panic
#' @export
panic <- function(...) {
    info(collapse_chr(...), type = "panic")
}

#' @rdname progress
#' @name itemize
#' @param .f TODO
#' @param .message TODO
#' @param .timer TODO
#' @importFrom purrr quietly
#' @export
itemize <- function(.f, ..., .message = NULL, .timer = TRUE) {
    function(...) {
        m1 <- paste("\u2500 ", .message, "...")
        n1 <- m1 %>% str_length()
        cat(m1, "\r")
        start_time <- Sys.time()
        # browser()
        result <- purrr::safely(quietly(.f))(...)
        result <- c(result$result, list(error = result$error))
        finish_time <- Sys.time()
        m2 <- .message
        n2 <- nchar(m2)
        if (.timer) {
            dt <- finish_time %>%
                difftime(start_time)
            if (as.numeric(dt, units = "secs") >= 0.5) {
                dt %<>%
                    prettyunits::pretty_dt() %>%
                    str_c("(", ., ")")
                n2 <- n2 + nchar(dt)
                m2 <- paste(m2, cli::col_cyan(dt))
            }
        }
        m2 %<>% str_pad(max(n1, n2 + 3), "right", " ")
        if (length(result$warnings) > 0 || length(result$error) > 0) {
            item(m2)
        } else {
            success(m2)
        }
        if (length(result$error) > 0) {
            panic(result$error$message)
            stop(call. = FALSE)
        }
        if (length(result$warnings) > 0) {
            for (i in seq_along(result$warnings)) {
                concern(result$warnings[[i]])
            }
        }
        if (length(result$messages) > 0) {
            for (i in seq_along(result$messages)) {
                result$messages[[i]] %>%
                    str_remove("\\n$") %>%
                    bullet(bullet = "square_small")
            }
        }
        result$result
    }
}

collapse_chr <- function(..., .sep = "", .quotes = FALSE) {
    x <- list(...) %>% unlist(FALSE, FALSE)
    if(is.character(x) && .quotes){
        x %<>% str_c("'", ., "'")
    }
    x %<>% str_c(collapse = .sep)
    if (length(x) == 0) return("")
    x
}

commas = function(..., .quotes = TRUE) {
    collapse_chr(..., .sep = ", ", .quotes = .quotes)
}

safely_and_quietly <- function(.f, otherwise = NULL, quiet = TRUE) {
    purrr::safely(quietly(.f), otherwise = NULL, quiet = TRUE)
}

info <- function(x = NULL, ..., type = c("item", "bullet", "success", "concern", "panic")) {
    type <- match.arg(type)
    dots <- list(...)
    dots$x <- str_c(" ", x)
    if (type == "bullet") {
        dots$bullet %<>% replace_null("bullet")
        dots$bullet_col %<>% replace_null("white")
        dots$col %<>% replace_null("white")
    }
    if (type == "item") {
        dots$bullet %<>% replace_null("line")
        dots$bullet_col %<>% replace_null("grey")
        dots$col %<>% replace_null("grey")
    }
    if (type == "success") {
        dots$bullet %<>% replace_null("tick")
        dots$bullet_col %<>% replace_null("green")
        dots$col %<>% replace_null("silver")
    }
    if (type == "concern") {
        dots$bullet %<>% replace_null("square_small_filled")
        dots$bullet_col %<>% replace_null("yellow")
        dots$col %<>% replace_null("yellow")
    }
    if (type == "panic") {
        dots$bullet %<>% replace_null("cross")
        dots$bullet_col %<>% replace_null("red")
        dots$col %<>% replace_null("red")
    }
    do.call(cli::cat_bullet, dots)
    invisible(list(info = x, type = type))
}
