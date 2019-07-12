
#' @rdname skim
#' @name skim
#' @title Summarise a data frame
#' @description Returns a \link[dplyr]{tibble} with rows corresponding to the variables in
#'   \code{x} and columns giving values for a range of summary statistics (result is
#'   returned silently).
#'   
#'   When \code{verbose = TRUE}, comments are printed to the console to highlight possible
#'   issues with data that might warrent further inspection prior to analysis.
#' @param x a data frame
# @param max_p_na ???
# @param min_distinct_num integer - a warning will be printed if any \code{dbl} or
#   \code{int} variables have fewer than this number of distinct (non-negative) values
# @param max_distinct_cat integer - a warning will be printed if any \code{chr},
#   \code{fct} or \code{ord} variables have fewer than this number of distinct
#   (non-negative) values
# @param min_distinct_mode integer - mode statistics will not be calculate for variables
#   with more than this number of distinct (non-negative) values
# @param max_p_mode proportion - a warning will be printed if the proportion of the model
#   value for any variables exceeds this value
#' @param verbose if \code{TRUE} messages will be printed to the console
#' @importFrom dplyr count n_distinct mutate_all one_of type_sum
#' @importFrom purrr map_df pmap set_names
#' @importFrom rlang flatten_chr flatten_dbl flatten_int
#' @export
#' @examples
#' \dontrun{
#' 
#' library(tidyverse)
#' 
#' as_tibble(iris)
#' results <- skim(iris)
#' print(results)
#'
#' as_tibble(mtcars)
#' results <- skim(mtcars)
#' print(results)
#'
#' as_tibble(esoph)
#' results <- skim(esoph)
#' print(results)
#'
#' as_tibble(attenu)
#' results <- skim(attenu)
#' print(results)
#'
#' nycflights13::airports
#' results <- skim(nycflights13::airports)
#' print(results)
#'
#' nycflights13::planes
#' results <- skim(nycflights13::planes)
#' print(results)
#'
#' nycflights13::weather
#' results <- skim(nycflights13::weather)
#' print(results)
#'
#' nycflights13::flights
#' results <- skim(nycflights13::flights)
#' print(results)
#' }

skim <- function(x,
                 verbose = TRUE) {
  stopifnot(is.data.frame(x))
  heading(deparse(substitute(x)))

  msg <- function(msg = "", vars = "", fun = concern) {
    vars %<>%
      sort %>%
      collapse_str(
        .sep = ", ",
        .use_quotes = FALSE,
        .width = getOption("width") - nchar(msg) - 1,
        .indent = 0,
        .exdent = nchar(msg) + 4
      )
    fun(msg, vars)
  }

  # df to store results
  out <- tibble(name = colnames(x))

  # size & dim -----
  N <- nrow(x)
  if (verbose) {
    dims <- dim(x) %>% format(big.mark = ",")
    comp = stats::complete.cases(x)
    ncomp = sum(comp)
    pcomp = round_str(100 * mean(comp), 1)
    if (ncomp < N && pcomp == "100.0") {
        pcomp = "99.9"
    }
    item("size:", utils::object.size(x) %>% format(units = "auto"))
    item("rows:", dims[1])
    item("cols:", dims[2])
    item("complete cases:", ncomp, str_c("(", pcomp, "%)"))
  }

  # col types -----
  if (verbose) bullet("col types")
  out$type <- x %>% map_chr(type_sum)
  if (verbose) {
    out %>%
      count(.data$type) %>%
      mutate_all(format, big.mark = ",") %>%
      pmap(function(type, n) {
        item(type, ":", n)
      }) %>%
      invisible()
  }
  valid_types <- c("chr", "fct", "ord", "lgl", "dbl", "int")
  valid <- out$type %in% valid_types
  if (all(!valid)) {
    return(invisible())
  }
  info <- out %>% filter(!valid)
  if (nrow(info) > 0) {
    msg("ignoring column types:", info$type)
  }
  out %<>% filter(valid)
  y <- x %>%
    select(which(valid)) %>%
    as_tibble()

  # uniques -----
  if (verbose) bullet("checking number of unique values...")
  out$nun <- y %>% map_int(n_distinct, na.rm = TRUE)
  if (verbose) {
    info <- out %>% filter(.data$nun %in% 1)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with only one unique value:"), info$name, panic)
    }
    info <- out %>% filter(.data$nun %in% 2)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with only two unique values:"), info$name, item)
    }
    # > min_distinct_num -----
    info <- out %>%
      filter(.data$type %in% c("dbl", "int")) %>%
      filter(.data$nun < 10)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars with < 10 unique values:"), info$name)
    }
    # > max_distinct_cat -----
    info <- out %>%
      filter(.data$type %in% c("chr", "fct", "ord")) %>%
      filter(.data$nun > 30)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " cat vars with > 30 unique values:"), info$name)
    }
  }

  # non-finite -----
  if (verbose) bullet("checking for non-finite values...")
  # > NA -----
  out$nna <- y %>% map_int(~ sum(is.na(.x)))
  out$pna <- out$nna / N
  if (verbose) {
    info <- out %>% filter(.data$pna > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NA values:"), info$name, panic)
    }
  }
  # > NaN -----
  out$nnan <- y %>% map_int(~ sum(is.nan(.x)))
  out$pnan <- out$nnan / N
  if (verbose) {
    info <- out %>% filter(.data$nnan > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NaN values:"), info$name, panic)
    }
  }
  # > Inf -----
  out$ninf <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x %in% c(-Inf, Inf)),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  out$pinf <- out$ninf / N
  if (verbose) {
    info <- out %>% filter(.data$ninf > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with inf values:"), info$name, panic)
    }
  }
  # > finite -----
  out %<>%
    mutate(
      nfin = N - .data$nna - .data$nnan - .data$ninf,
      pfin = .data$nfin / N
    )
  info <- out %>% filter(.data$nfin == 0)
  if (nrow(info) > 0) {
    msg(str_c(nrow(info), " vars with no finite values:"), info$name, panic)
  }
  info <- out %>% filter(.data$pfin < 0.5)
  if (nrow(info) > 0) {
    msg(str_c(nrow(info), " vars with < 0.5 finite values:"), info$name)
  }

  # distribution -----
  if (verbose) bullet("checking distributions...")
  # > pos -----
  out$npos <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x > 0, na.rm = TRUE),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  # > zero -----
  out$nzero <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x == 0, na.rm = TRUE),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  # > neg -----
  out$nneg <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x < 0, na.rm = TRUE),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  if (verbose) {
    # strictly +ve
    info <- out %>%
      filter(.data$npos > 0) %>%
      filter(.data$nzero == 0) %>%
      filter(.data$nneg == 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars are all +ve:"), info$name, item)
    }
    # non-negative (+ve or zero)
    info <- out %>%
      filter(.data$npos > 0) %>%
      filter(.data$nzero > 0) %>%
      filter(.data$nneg == 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars are non -ve:"), info$name, item)
    }
    # only zero
    info <- out %>%
      filter(.data$npos == 0) %>%
      filter(.data$nzero > 0) %>%
      filter(.data$nneg == 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars are al zero:"), info$name, item)
    }
    # non-+ve (i.e. -ve or zero)
    info <- out %>%
      filter(.data$npos == 0) %>%
      filter(.data$nzero > 0) %>%
      filter(.data$nneg > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars are non +ve:"), info$name, item)
    }
    # strictly -ve
    info <- out %>%
      filter(.data$npos == 0) %>%
      filter(.data$nzero == 0) %>%
      filter(.data$nneg > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars are all -ve:"), info$name, item)
    }
  }
  # > max -----
  out$max <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ .x %>%
        max(na.rm = TRUE) %>%
        as.numeric(),
      # ~ .x %>% keep(is.finite(.x)) %>% max(.x),
      .else = ~NA_real_
    ) %>%
    flatten_dbl()
  # > min -----
  out$min <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ .x %>%
        min(na.rm = TRUE) %>%
        as.numeric(),
      .else = ~NA_real_
    ) %>%
    flatten_dbl()
  # > binomial -----
  if (verbose) {
      info <- out %>%
          filter(.data$nun == 2) %>%
          filter(.data$min == 0) %>%
          filter(.data$max == 1)
      if (nrow(info) > 0) {
          msg(str_c(nrow(info), " num vars that might be binary:"), info$name, item)
      }
  }
  # > mean -----
  out$mean <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int", "lgl"),
      ~ .x %>%
        mean(na.rm = TRUE) %>%
        as.numeric(),
      .else = ~NA_real_
    ) %>%
    flatten_dbl()
  # > sd -----
  out$sd <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int", "lgl"),
      ~ .x %>%
        sd(na.rm = TRUE) %>%
        as.numeric(),
      .else = ~NA_real_
    ) %>%
    flatten_dbl()
  if (verbose) {
    info <- out %>%
      filter(.data$sd < 0.01)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars with sd < 0.01:"), info$name, item)
    }
  }
  # > mode -----
  modes <- y %>%
    map_if(
      ~ type_sum(.x) %in% c("chr", "fct", "ord"),
      ~ .x %>%
        table() %>%
        {
          .[which.max(.)]
        },
      # ~ .x %>% keep(is.finite(.x)) %>% max(.x),
      .else = ~NULL
    )
  out$mode <- modes %>%
    map_if(~ !is.null(names(.x)), ~ names(.x), .else = ~NA_character_) %>%
    flatten_chr()
  out$nmode <- modes %>%
    unname() %>%
    map_if(~ !is.null(.x), ~ unname(.x), .else = ~NA_integer_) %>%
    flatten_int()
  out$pmode <- NA_real_
  i <- !is.na(out$nmode)
  out$pmode[i] <- out$nmode[i] / (N - out$nna[i])
  if (verbose) {
    info <- out %>%
      filter(.data$pmode > 0.9)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " cat vars with prop mode > 0.9:"), info$name, item)
    }
  }

  # out %>% colnames %>% str_c('"', ., '",') %>% writeLines()
  out %>%
    select(one_of(
      "name",
      "type",
      "nun",
      "nna",
      "nnan",
      "ninf",
      "nfin",
      "npos",
      "nzero",
      "nneg",
      "max",
      "min",
      "mean",
      "sd",
      "mode",
      "nmode",
      "pmode",
      "pna",
      "pnan",
      "pinf",
      "pfin",
      character(0)
    )) %>%
    invisible()


  # out %<>%
  #     mutate(
  #         min_distinct_mode = (.data$n <= !!min_distinct_mode) %in% TRUE,
  #         table = vector("list", ncol(x))
  #     )
  # i = out$min_distinct_mode
  # out$table[i] = x[,i] %>% map(table)
  # out %<>%
  #     mutate(
  #         mode = .data$table %>%
  #             map_chr(function(x){
  #                 if(is.null(x)) return(NA_character_)
  #                 names(x)[which.max(x)]
  #             }),
  #         n_mode = table %>%
  #             map_int(function(x){
  #                 if(is.null(x)) return(NA_integer_)
  #                 max(x)
  #             }),
  #         p_mode = .data$n_mode / nrow(x)
  #     ) %>%
  #     select(-.data$table,
  #            -.data$min_distinct_mode)
  # if(verbose){
  #     # mode == 0
  #     temp = out %>%
  #         filter(.data$type %in% c("dbl","int")) %>%
  #         filter((.data$p_mode > max_p_mode) %in% TRUE)
  #     if(nrow(temp) > 0){
  #         warn(nrow(temp), "num vars have mode = 0")
  #     }
  #     # p_mode > max_p_mode
  #     temp = out %>%
  #         filter((.data$p_mode > max_p_mode) %in% TRUE)
  #     if(nrow(temp) > 0){
  #         warn(nrow(temp), "vars have p_mode > ", max_p_mode)
  #     }
  # }
  #
  # invisible(out)
}
