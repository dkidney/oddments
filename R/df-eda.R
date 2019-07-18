
#' @rdname df-eda
#' @name df-eda
#' @title Explore a data frame
#' @description
#' \itemize{
#'
#'   \item \code{skim()} - returns a \link[dplyr]{tibble} with rows corresponding to the
#'   variables in \code{data} and columns giving values for a range of summary statistics
#'   (the result is returned silently). When \code{verbose = TRUE}, messages are printed
#'   to the console to provide high-level insight and highlight possible issues with data
#'   that might warrent further inspection prior to analysis.
#'
#'   \item \code{sketch()} - plots the distribution of a variable (or combination of
#'   variables) in \code{data} - the relationship with a second variable (or combination
#'   of variables) can be overlayed and faceting by a thrid variable (or combination
#'   of variables) is also possible.
#'
#' }
#' @details \link[dplyr]{type_sum} is used to determine the class of the \code{x}
#'   variable:
#'   \itemize{
#'     \item if \code{x} is one of \code{"chr"}, \code{"fct"}, \code{"lgl"} or
#'     \code{"ord"} then it is treated as discrete
#'     \item if \code{x} is one of \code{"dbl"} or \code{"int"} then it is treated as
#'     non-discrete
#'     \item no other variable types are supported for \code{x}
#'   }
#'
#'   \link[ggplot2]{geom_histogram} is used to plot non-discrete x variables and
#'   \link[ggplot2]{geom_bar} is used to plot discrete x variables.
#' @param data a data frame
NULL

#' @rdname df-eda
#' @name skim
#' @param verbose if \code{TRUE} messages will be printed to the console
#' @importFrom dplyr count n_distinct mutate_all one_of type_sum
#' @importFrom purrr map_df pmap set_names
#' @importFrom rlang flatten_chr flatten_dbl flatten_int
#' @export
skim <- function(data,
                 verbose = TRUE) {
  stopifnot(is.data.frame(data))
  heading(deparse(substitute(data)))

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
  out <- tibble(name = colnames(data))

  # size & dim -----
  N <- nrow(data)
  if (verbose) {
    dims <- dim(data) %>% format(big.mark = ",")
    comp = stats::complete.cases(data)
    ncomp = sum(comp)
    pcomp = round_str(100 * mean(comp), 1)
    if (ncomp < N && pcomp == "100.0") {
        pcomp = "99.9"
    }
    item("size:", utils::object.size(data) %>% format(units = "auto"))
    item("rows:", dims[1])
    item("cols:", dims[2])
    item("complete cases:", ncomp, str_c("(", pcomp, "%)"))
  }

  # col types -----
  if (verbose) bullet("col types")
  out$type <- data %>% map_chr(type_sum)
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
  x <- data %>%
    select(which(valid)) %>%
    as_tibble()

  # uniques -----
  if (verbose) bullet("checking number of unique values...")
  out$nun <- x %>% map_int(n_distinct, na.rm = TRUE)
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
  out$nna <- x %>% map_int(~ sum(is.na(.x)))
  out$pna <- out$nna / N
  if (verbose) {
    info <- out %>% filter(.data$pna > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NA values:"), info$name, panic)
    }
  }
  # > NaN -----
  out$nnan <- x %>% map_int(~ sum(is.nan(.x)))
  out$pnan <- out$nnan / N
  if (verbose) {
    info <- out %>% filter(.data$nnan > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NaN values:"), info$name, panic)
    }
  }
  # > Inf -----
  out$ninf <- x %>%
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
  out$npos <- x %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x > 0, na.rm = TRUE),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  # > zero -----
  out$nzero <- x %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int"),
      ~ sum(.x == 0, na.rm = TRUE),
      .else = ~NA_integer_
    ) %>%
    flatten_int()
  # > neg -----
  out$nneg <- x %>%
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
  out$max <- x %>%
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
  out$min <- x %>%
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
  out$mean <- x %>%
    map_if(
      ~ type_sum(.x) %in% c("dbl", "int", "lgl"),
      ~ .x %>%
        mean(na.rm = TRUE) %>%
        as.numeric(),
      .else = ~NA_real_
    ) %>%
    flatten_dbl()
  # > sd -----
  out$sd <- x %>%
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
  modes <- x %>%
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
}

#' @rdname df-eda
#' @name sketch
#' @export
#' @param x variable for which the distribution will be plotted
# @param y optional y variable
#' @param group optional grouping variable
#' @param min_distinct_xnum integer, if \code{x} is non-discrete and the number of
#'   distinct values in \code{x} is less than \code{min_distinct_xnum} a error will be
#'   thrown (see Details)
#' @param max_distinct_xcat integer, if \code{x} is discrete and the number of distinct
#'   values in \code{x} exceeds \code{max_distinct_xcat} a error will be thrown (see
#'   Details)
#' @param max_distinct_group integer, if the number of distinct values in \code{group}
#'   exceeds \code{max_distinct_group} a error will be thrown
#' @param bins passed to \link[ggplot2]{geom_histogram} (ignored if \code{x} is
#'   discrete)
#' @importFrom dplyr group_by n summarise transmute ungroup
#' @importFrom ggplot2 aes facet_wrap ggplot_build labs scale_y_continuous sec_axis
#' @importFrom ggplot2 ggplot geom_bar geom_point geom_smooth geom_line geom_ribbon
#' @importFrom rlang enquo quo_name
sketch <- function(data,
                   x,
                   y,
                   group,
                   min_unique_continuous = 10,
                   max_unique_discrete = 50,
                   max_distinct_group = 3,
                   x_bins = 30,
                   y_method = NULL,
                   y_family = NULL,
                   y_points = NULL,
                   k = 10) {
    stopifnot(inherits(data, "data.frame"))
    stopifnot(!missing(x))
    use_y <- !missing(y)
    use_group <- !missing(group)

    # data trans -----
    quos <- list(x = enquo(x))
    if (use_y) quos$y <- enquo(y)
    if (use_group) quos$group <- enquo(group)
    data_trans <- data %>%
        as_tibble() %>%
        transmute(!!!quos)

    # var names -----
    var_names <- list(x = quo_name(quos$x))
    # if(use_y) var_names$y = quo_name(quos$y)
    if (use_group) var_names$group <- quo_name(quos$group)

    # check x -----
    x_type <- data_trans$x %>% type_sum()
    if (!x_type %in% c("chr", "dbl", "fct", "int", "lgl", "ord")) {
        # if(!x_type %in% c("chr", "date", "dbl", "fct", "int", "lgl", "ord")){
        stop(paste0("cant plot x variable type '", x_type, "'"))
    }
    continuous_x <- x_type %in% c("dbl", "int")
    # continuous_x = x_type %in% c("dbl", "int", "date")
    # x_is_date = x_type %in% "date"

    # check y -----
    if (use_y) {
        y_type <- data_trans$y %>% type_sum()
        if (!y_type %in% c("dbl", "int")) {
            warn(str_c("cant plot y variable type '", y_type, "'"))
            use_y <- FALSE
        }
    }
    if (use_y) {
        if (is.null(y_method)) {
            y_method <- if (continuous_x) "gam" else "glm"
        }
        y_methods <- if (continuous_x) {
            c("gam", "glm", "loess", "none")
        } else {
            c("glm", "boxplot", "none")
        }
        if (!y_method %in% y_methods) {
            warning(
                "expecting y_method to be one of: '",
                str_c(y_methods, collapse = "', '"), "'"
            )
            y_method <- "none"
        }
    }
    if (use_y) {
        if (is.null(y_points)) {
            y_points <- FALSE
        }
        y_points <- isTRUE(y_points)
        if (!y_points && y_method == "none") {
            use_y <- FALSE
        }
    }
    if (use_y) {
        y_smooth <- y_method != "none"
        if (y_smooth) {
            if (is.null(y_family)) {
                y_family <- if (y_method == "loess") "gaussian" else guess_family(data_trans$y)
            }
            if (y_family == "binomial" && y_method == "boxplot") {
                warn(str_c("boxplot method unsuitable for y variable type ", y_family))
            }
        }
    }

    # check group -----
    if (use_group) {
        if (n_distinct(data_trans$group) > max_distinct_group) {
            stop(paste("group has more than", max_distinct_group, "distinct values"))
        }
        data_trans$group %<>% as.factor
        levels(data_trans$group) %<>% paste(var_names$group, "=", .)
    } else {
        data_trans$group <- 1
    }
    group_fct <- levels(as.factor(data_trans$group)) %>% factor(., .)

    # data x -----
    if (continuous_x) {
        data_x <- data_trans %>%
            ggplot2::ggplot(.) +
            ggplot2::geom_histogram(aes(.data$x, group = .data$group), bins = x_bins)
        data_x %<>%
            ggplot_build() %>%
            pluck("data") %>%
            pluck(1) %>%
            as_tibble() %>%
            mutate(group = group_fct[.data$group]) %>%
            mutate(width = .data$xmax - .data$xmin) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$count / sum(.data$count, na.rm = TRUE)) %>%
            ungroup() %>%
            select(one_of("x", "prop", "group", "width"))
        # if(x_is_date){
        #     data_x$x %<>% as.Date(origin = "1970-01-01 UTC")
        # }
    } else {
        data_x <- data_trans %>%
            group_by(.data$x, .data$group) %>%
            summarise(n = n()) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$n / sum(.data$n), width = 0.9) %>%
            ungroup()
    }

    # plot x -----
    suppressWarnings({
        plot <- data_x %>%
            ggplot() +
            geom_bar(
                aes(.data$x, .data$prop, group = .data$group, width = .data$width),
                stat = "identity",
                fill = "dodgerblue",
                alpha = 0.8
            ) +
            labs(title = var_names$x, y = "Density", x = var_names$x) +
            theme(legend.position = "none")
    })
    if (use_group) {
        plot <- plot + facet_wrap(~group, ncol = 1)
    }

    # plot y -----
    if (use_y) {

        # helper functions for making second y axis
        y1_to_y2 <- function(y1) (y1 / y1_max * y2_rng) + y2_min
        y2_to_y1 <- function(y2) (y2 - y2_min) / y2_rng * y1_max

        if (continuous_x) {
            # k %<>% as.integer %>% check_is_scalar_integer()

            data_x <- suppressMessages({
                ggplot_build(plot)$data[[1]] %>% as_tibble()
            })

            plot_y <- data_trans %>%
                ggplot(aes(.data$x, .data$y)) +
                geom_smooth(
                    method = y_method,
                    method.args = list(family = y_family),
                    formula = if (y_method == "gam") {
                        formula(str_c("y ~ s(x, k = ", k, ")"))
                    } else {
                        y ~ x
                    }
                )

            data_y <- suppressMessages({
                ggplot_build(plot_y)$data[[1]] %>% as_tibble()
            })

            if (nrow(data_y) > 0) {

                # y2 values
                y1_max <- max(data_x$y)
                y2_min <- min(data_y$ymin)
                y2_max <- max(data_y$ymax)
                y2_rng <- y2_max - y2_min
                data_y %<>% mutate_at(c("y", "ymin", "ymax"), y2_to_y1)
                data_trans %<>% mutate_at("y", y2_to_y1)

                # y2 points
                if (y_points) {
                    y2_min %<>% min(data_trans$y)
                    y2_max %<>% max(data_trans$y)
                    plot <- plot +
                        geom_point(aes(.data$x, .data$y), data_trans, col = "red")
                }

                # y2 ribbon
                plot <- plot +
                    geom_line(aes(.data$x, .data$y), data_y, col = "red") +
                    geom_ribbon(aes(.data$x, ymin = .data$ymin, ymax = .data$ymax), data_y,
                                fill = "red", alpha = 0.2
                    ) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))
            }
        } else {
            binary_y = is.integer(data_trans$y) &&
                min(data_trans$y, na.rm == TRUE) == 0L &&
                max(data_trans$y, na.rm == TRUE) == 1L
            if (binary_y) {
                data_trans %<>%
                    group_by(.data$x) %>%
                    summarise(
                        n = .data$y %>% length(),
                        mean = .data$y %>% mean(),
                        nbad = .data$y %>% sum()
                    )

                # exact confidence intervals
                binom_ci <- binom.confint(data_trans$nbad, data_trans$n, methods = "exact")
                data_trans$lower <- binom_ci$lower
                data_trans$upper <- binom_ci$upper

                # y2 values
                y1_max <- max(counts_x$n)
                y2_min <- min(data_trans$lower)
                y2_max <- max(data_trans$upper)
                y2_rng <- y2_max - y2_min
                data_trans %<>% mutate_at(c("mean", "lower", "upper"), y2_to_y1)

                # y2 errorbars
                plot <- plot +
                    # geom_line(aes(.data$x, .data$mean, group = 1), data_trans, col = "red") +
                    geom_point(aes(.data$x, .data$mean), data_trans, col = "red") +
                    geom_errorbar(aes(.data$x, ymin = .data$lower, ymax = .data$upper), data_trans,
                                  colour = "red", width = 0.5
                    ) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))
            } else {
                y1_max <- max(counts_x$n)
                y2_min <- min(data_trans$y)
                y2_max <- max(data_trans$y)
                y2_rng <- y2_max - y2_min
                data_trans %<>% mutate_at("y", y2_to_y1)

                plot <- plot +
                    geom_boxplot(aes(.data$x, .data$y, group = .data$x), data_trans, col = "red", fill = NA) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))
            }
        }
    }

    plot +
        labs(x = substitute(x), y = "count")
}

guess_family <- function(x) {
    type <- x %>% type_sum()
    family <- NULL
    if (type == "lgl") {
        family <- "binomial"
    }
    if (is.null(family) &&
        min(x, na.rm = TRUE) == 0 &&
        max(x, na.rm = TRUE) == 1 &&
        n_distinct(x, na.rm = TRUE) == 2) {
        family <- "binomial"
    }
    if (is.null(family) &&
        type == "int" &&
        min(x, na.rm = TRUE) >= 0) {
        family <- "poisson"
    }
    if (is.null(family) &&
        type == "int" &&
        min(x, na.rm = TRUE) >= 0) {
        family <- "quasipoisson"
    }
    if (is.null(family) &&
        type == "dbl" &&
        min(x, na.rm = TRUE) >= 0 &&
        max(x, na.rm = TRUE) <= 1) {
        family <- "quasibinomial"
    }
    if (is.null(family) &&
        type == "dbl" &&
        min(x, na.rm = TRUE) > 0) {
        family <- "Gamma"
    }
    if (is.null(family)) {
        family <- "gaussian"
    }
    family
}
