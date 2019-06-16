
#' @name oddments-package
#' @aliases oddments
#' @docType package
#' @title Data Science Tools
#' @description A data science toolbox.

#' @importFrom binom binom.confint
#' @import dplyr
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_inorder
#' @import ggplot2
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#' @importFrom lubridate now
#' @importFrom purrr discard
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr pluck
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom rlang %||%
#' @importFrom rlang has_name
#' @importFrom rlang is_scalar_character
#' @importFrom rlang is_scalar_integer
#' @importFrom rlang is_scalar_logical
#' @importFrom rlang is_scalar_double
#' @importFrom rlang set_names
#' @importFrom stats quantile
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_flatten
#' @importFrom stringr str_pad
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom tibble column_to_rownames
#' @importFrom utils capture.output
#' @importFrom utils installed.packages
#' @importFrom utils sessionInfo

#' @useDynLib oddments, .registration = TRUE
#' @importFrom Rcpp sourceCpp

utils::globalVariables(".")

NULL