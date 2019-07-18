
#' @name pip-package
#' @aliases pip
#' @docType package
#' @title Predictor Insight Plots.
#' @description Predictor insight plots.
# @section Issues:
# \describe{
#   \item{comparison}{function to compare train and test - e.g. check factor levels the
#   same, numeric limits the same, missing values in one but not the other. Print results to console.}
#   \item{correlation}{cramers_v?}
#   \item{rmarkdown template}{}
# }
#' @example inst/examples/examples.R

# @importFrom binom binom.confint
#' @importFrom dplyr any_vars
#' @importFrom dplyr arrange
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr data_frame
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#' @importFrom dplyr enquo
#' @importFrom dplyr filter
#' @importFrom dplyr filter_all
#' @importFrom dplyr funs
#' @importFrom dplyr group_by
#' @importFrom dplyr quo_name
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr type_sum
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_explicit_na
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 cut_interval
#' @importFrom ggplot2 cut_number
#' @importFrom ggplot2 cut_width
#' @importFrom ggplot2 dup_axis
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 rel
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme_set
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom purrr compact
#' @importFrom purrr keep
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr map_lgl
#' @importFrom purrr pluck
# @importFrom purrr set_names
# @importFrom rlang !!
#' @importFrom rlang .data
# @importFrom rlang %||%
#' @importFrom rlang has_name
#' @importFrom rlang is_scalar_character
# @importFrom rlang is_scalar_integer
# @importFrom rlang is_scalar_logical
# @importFrom rlang is_scalar_double
#' @importFrom rlang set_names
#' @importFrom rlang warn
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats plogis
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stringr str_c
# @importFrom stringr str_detect
# @importFrom stringr str_flatten
# @importFrom stringr str_pad
# @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
# @importFrom tibble column_to_rownames
# @importFrom utils capture.output
# @importFrom utils installed.packages
# @importFrom utils sessionInfo

if(getRversion() >= "2.15.1") utils::globalVariables(".")

NULL
