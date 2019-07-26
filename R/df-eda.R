
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
#' @param data a data frame
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
#' @example inst/examples/examples-df-eda.R
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
    comp <- stats::complete.cases(data)
    ncomp <- sum(comp)
    pcomp <- round_str(100 * mean(comp), 1)
    if (ncomp < N && pcomp == "100.0") {
      pcomp <- "99.9"
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
    # min unique num
    info <- out %>%
      filter(.data$type %in% c("dbl", "int")) %>%
      filter(.data$nun < 10)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " num vars with < 10 unique values:"), info$name)
    }
    # max unique cat
    info <- out %>%
      filter(.data$type %in% c("chr", "fct", "ord")) %>%
      filter(.data$nun > 30)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " cat vars with > 30 unique values:"), info$name)
    }
  }
  
  # non-finite -----
  if (verbose) bullet("checking for non-finite values...")
  # NA
  out$nna <- x %>% map_int(~ sum(is.na(.x)))
  out$pna <- out$nna / N
  if (verbose) {
    info <- out %>% filter(.data$pna > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NA values:"), info$name, panic)
    }
  }
  # NaN
  out$nnan <- x %>% map_int(~ sum(is.nan(.x)))
  out$pnan <- out$nnan / N
  if (verbose) {
    info <- out %>% filter(.data$nnan > 0)
    if (nrow(info) > 0) {
      msg(str_c(nrow(info), " vars with NaN values:"), info$name, panic)
    }
  }
  # Inf
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
  # finite
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
        {.[which.max(.)]},
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
#' @param x unquoted expression giving the variable to be plotted (can be a combination of
#'   one of more variables in data)
#' @param y TODO
#' @param group optional grouping variable
#' @param min_unique_continuous integer, if \code{x} is non-discrete and the number of
#'   distinct values in \code{x} is less than \code{min_distinct_xnum} a error will be
#'   thrown (see Details)
#' @param max_unique_discrete integer, if \code{x} is discrete and the number of distinct
#'   values in \code{x} exceeds \code{max_distinct_xcat} a error will be thrown (see
#'   Details)
#' @param max_distinct_group integer, if the number of distinct values in \code{group}
#'   exceeds \code{max_distinct_group} a error will be thrown
#' @param x_bins passed to \link[ggplot2]{geom_histogram} (ignored if \code{x} is
#'   discrete)
#' @param x_binwidth TODO
#' @param x_col TODO
#' @param y_method TODO
#' @param y_family TODO
#' @param y_points TODO
#' @param y_col TODO
#' @param k TODO
#' @importFrom dplyr group_by n summarise transmute ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 sec_axis
#' @importFrom rlang enquo quo_name
sketch <- function(data,
                   x,
                   y,
                   group,
                   min_unique_continuous = 10,
                   max_unique_discrete = 50,
                   max_distinct_group = 3,
                   x_bins = NULL,
                   x_binwidth = NULL,
                   x_col = "dodgerblue",
                   y_points = NULL,
                   y_method = NULL,
                   y_family = NULL,
                   y_col = "grey30",
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
  if (use_y) var_names$y <- quo_name(quos$y)
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
      warning(str_c("cant plot y variable type '", y_type, "'"))
      use_y <- FALSE
    }
  }
  if (use_y) {
    y_family_guess <- guess_family(data_trans$y)
    if (continuous_x) {
      y_methods <- c("gam", "glm", "loess", "none")
      if (y_family_guess == "binomial") {
        y_method %<>% replace_null("glm")
      } else {
        y_method %<>% replace_null("loess")
      }
    } else {
      y_methods <- c("boxplot", "glm", "none")
      if (y_family_guess == "binomial") {
        y_method %<>% replace_null("glm")
      } else {
        y_method %<>% replace_null("boxplot")
      }
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
    y_smooth <- y_method != "none"
    if (y_smooth) {
      if (y_method == "loess") {
        y_family %<>% replace_null("gaussian")
      } else {
        y_family %<>% replace_null(y_family_guess)
      }
      if (y_method == "loess" && y_family_guess == "binomial") {
        warning("y looks to be binary - consider using y_method = 'glm'")
      }
      if (y_family == "binomial" && y_method == "boxplot") {
        warning("boxplot method unsuitable for y_family = 'binomial'")
      }
    }
  }
  if (use_y) {
    if (is.null(y_points)) {
      y_points <- TRUE
    }
    y_points <- isTRUE(y_points)
    if (!y_points && y_method == "none") {
      use_y <- FALSE
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
  # this section prepares x data for plotting using geom_bar
  # if continuous, then use geom_histogram to discretise x and get calculate totals
  # otherwise calculate totals for each level of x
  if (continuous_x) {
    if (is.null(x_bins) && is.null(x_binwidth)) {
      x_bins <- 30
    }
    data_x <- data_trans %>%
      ggplot() +
      geom_histogram(
        mapping = aes(.data$x, group = .data$group),
        binwidth = x_binwidth,
        bins = x_bins
      )
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
    plt <- data_x %>%
      ggplot() +
      geom_bar(
        aes(.data$x, .data$prop, group = .data$group, width = .data$width),
        stat = "identity",
        fill = x_col,
        alpha = 0.8
      ) +
      labs(
        # title = var_names$x,
        y = str_c(var_names$x, " density"),
        x = var_names$x
      ) +
      theme(legend.position = "none")
  })
  if (use_group) {
    plt <- plt + facet_wrap(~group, ncol = 1)
  }
  if (!use_y) {
    return(plt)
  }
  
  # plot y -----
  
  # helper functions for making second y axis
  y1_max <- max(data_x$prop)
  y2_min <- y2_max <- y2_dif <- NA_real_
  y1_to_y2 <- function(y1) (y1 / y1_max * y2_dif) + y2_min
  y2_to_y1 <- function(y2) (y2 - y2_min) / y2_dif * y1_max
  
  plot_y_temp <- data_trans %>%
    ggplot(aes(.data$x, .data$y, group = .data$group))
  
  if (y_points) {
    plot_y_points <- plot_y_temp + geom_point()
    data_y_points <- suppressMessages({
      plot_y_points %>%
        ggplot_build() %>%
        pluck("data") %>%
        pluck(1) %>%
        as_tibble() %>%
        mutate(group = group_fct[.data$group]) %>%
        select(.data$x, starts_with("y"), .data$group)
    })
    y2_min %<>% min(data_y_points$y, na.rm = TRUE)
    y2_max %<>% max(data_y_points$y, na.rm = TRUE)
    y2_dif <- y2_max - y2_min
  }
  
  if (continuous_x) {
    if (y_smooth) {
      plot_y_smooth <- plot_y_temp +
        geom_smooth(
          method = y_method,
          method.args = list(family = y_family),
          formula = if (y_method == "gam") {
            stats::formula(str_c("y ~ s(x, k = ", k, ")"))
          } else {
            y ~ x
          }
        )
      data_y_smooth <- suppressWarnings({
        suppressMessages({
          plot_y_smooth %>%
            ggplot_build() %>%
            pluck("data") %>%
            pluck(1) %>%
            as_tibble() %>%
            mutate(group = group_fct[.data$group]) %>%
            select(.data$x, starts_with("y"), .data$group)
        })
      })
      y2_lims <- data_y_smooth %>%
        select(starts_with("y")) %>%
        map(range, na.rm = TRUE) %>%
        unname() %>%
        unlist()
      y2_min %<>% min(y2_lims, na.rm = TRUE)
      y2_max %<>% max(y2_lims, na.rm = TRUE)
      y2_dif <- y2_max - y2_min
    }
    
    if (y_smooth) {
      data_y_smooth %<>%
        mutate_at(c("y", "ymin", "ymax"), y2_to_y1)
      plt <- plt +
        geom_line(
          mapping = aes(.data$x, .data$y),
          data = data_y_smooth,
          col = y_col
        ) +
        geom_ribbon(
          mapping = aes(.data$x, ymin = .data$ymin, ymax = .data$ymax),
          data = data_y_smooth,
          fill = y_col, alpha = 0.2
        )
    }
  }
  if (!continuous_x) {
    if (y_smooth) {
      if (y_method == "glm") {
        data_y_smooth <- data_trans %>%
          tidyr::nest(-group) %>%
          mutate(
            data = data %>%
              map(function(data) {
                model <- stats::glm(y ~ x, y_family, data)
                pred_data <- select(data, x) %>% dplyr::distinct()
                preds <- model %>% stats::predict(pred_data, se.fit = TRUE)
                fam <- stats::family(model)
                pred_data %>%
                  mutate(
                    y = fam$linkinv(preds$fit),
                    ymin = fam$linkinv(preds$fit - 1.96 * preds$se.fit),
                    ymax = fam$linkinv(preds$fit + 1.96 * preds$se.fit)
                  )
              })
          ) %>%
          tidyr::unnest() %>%
          mutate(group = group_fct[.data$group]) %>%
          select(.data$x, starts_with("y"), .data$group)
        y2_lims <- data_y_smooth %>%
          select(starts_with("y")) %>%
          map(range, na.rm = TRUE) %>%
          unname() %>%
          unlist()
        y2_min %<>% min(y2_lims, na.rm = TRUE)
        y2_max %<>% max(y2_lims, na.rm = TRUE)
        y2_dif <- y2_max - y2_min
        data_y_smooth %<>%
          mutate_at(c("y", "ymin", "ymax"), y2_to_y1)
        # y2 errorbars
        plt <- plt +
          geom_point(
            aes(.data$x, .data$y),
            data = data_y_smooth,
            col = y_col
          ) +
          geom_errorbar(
            aes(.data$x, ymin = .data$ymin, ymax = .data$ymax),
            data = data_y_smooth,
            colour = y_col, width = 0.5
          )
      }
      if (y_method == "boxplot") {
        y2_min <- min(data_trans$y)
        y2_max <- max(data_trans$y)
        y2_dif <- y2_max - y2_min
        data_trans %<>%
          mutate_at("y", y2_to_y1)
        plt <- plt +
          geom_boxplot(
            mapping = aes(.data$x, .data$y, group = .data$x),
            data = data_trans,
            col = y_col, fill = NA
          )
      }
    }
    if (y_points) {
      data_y_points %<>%
        mutate_at("x", jitter, 0.1)
    }
  }
  if (y_points) {
    data_y_points %<>%
      mutate_at("y", y2_to_y1)
    plt <- plt +
      geom_point(
        mapping = aes(.data$x, .data$y, group = .data$group),
        data = data_y_points,
        col = y_col, alpha = 0.3
      )
  }
  if (y_smooth) {
    plt <- plt +
      labs(subtitle = str_c("y family = ", y_family, ", y method = ", y_method))
  }
  plt + scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~ y1_to_y2(.),
      name = substitute(y)
    )
  )
}

guess_family <- function(x) {
  type <- x %>% type_sum()
  if (type == "lgl") {
    return("binomial")
  }
  if (min(x, na.rm = TRUE) == 0 &&
      max(x, na.rm = TRUE) == 1 &&
      n_distinct(x, na.rm = TRUE) == 2) {
    return("binomial")
  }
  if (type == "int" &&
      min(x, na.rm = TRUE) >= 0) {
    return("poisson")
  }
  if (type == "dbl" &&
      min(x, na.rm = TRUE) >= 0 &&
      max(x, na.rm = TRUE) <= 1) {
    return("quasibinomial")
  }
  if (type == "dbl" &&
      min(x, na.rm = TRUE) > 0) {
    return("Gamma")
  }
  if (type == "dbl" &&
      min(x, na.rm = TRUE) >= 0) {
    return("quasipoisson")
  }
  "gaussian"
}
