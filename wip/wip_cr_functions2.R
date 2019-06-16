
# @title Calculate WoE values
# @description ???
# @param y binary response vector (1 = bad, 0 = good)
# @param x predictor variable vector
# @param w ???
# @param adjust ???
# @param min.total ???
# @param min.bads ???
# @param woe.min.range ???
# @param woe.min.level ???
# @param woe.ignore.level ???
# @export
# @examples
# \dontrun{
#
# n = 1e6
# df = data_frame(
#    x = sample(letters[1:5], n, replace = TRUE),
#    y = rbinom(n, 1, prob = 0.5)
#)
#df %>% Information::create_infotables(y = "y")
#df %$% woe(x, y)
#}

# woe = function(
#     x,
#     y,
#     # w = NULL,
#     adjust = 0
#     # min.total = 1,
#     # min.bads = 0,
#     # woe.min.range = 0,
#     # woe.min.level = 0,
#     # woe.ignore.level = NULL
# ){
#
#     # checks
#     # if(length(x) != length(y)) stop("length x not equal to length y")
#     # if(!inherits(y, c("integer", "numeric")) ||
#     #    any(!unique(y, na.rm = TRUE) %in% c(0,1)))
#     #     stop("y must be a numeric or integer vector of binary values")
#     # if(!inherits(x, c("character", "factor")))
#     #     return(data_frame(n = length(x)))
#     # # check values
#     # if(!all(y %in% c(0,1))) stop("y must be a binary variable")
#
#     # calculate weighted stats ----
#     data_frame(
#         x = x %>% as.factor,
#         y = y
#     ) %>%
#         group_by(.data$x) %>%
#         summarise(
#             n.bad  = sum(.data$y %in% 1),
#             n.good = sum(.data$y %in% 0)
#         ) %>%
#         ungroup() %>%
#         mutate(
#             n = .data$n.bad + .data$n.good,
#             N = sum(.data$n),
#             N.bad  = sum(.data$n.bad),
#             N.good = sum(.data$n.good),
#             bad.rate = .data$n.bad / .data$n,
#             prop.bad  = (.data$n.bad + adjust) / .data$N.bad,
#             prop.good = (.data$n.good + adjust) / .data$N.good,
#             woe = log(.data$prop.bad / .data$prop.good)
#             # woe = woe %>%
#             #     if_else(n.bad < min.bads, 0, .) %>%
#             #     if_else(n < min.total, 0, .) %>%
#             #     if_else(abs(woe) < woe.min.level, 0, .),
#             # woe = if(abs(diff(range(woe))) < woe.min.range) 0 else woe,
#             # woe = ifelse(x %in% woe.ignore.level, 0, woe)
#         ) %>%
#         select(.data$x, .data$n, .data$prop.good, .data$woe)
#         # mutate_if(colnames(.) != "x", funs(ifelse(is.finite(.), ., NA))) %>%
#         # rename(level = x) %>%
#         # mutate(level.order = row_number()) %>%
#         # select(level, level.order, n, proportion, everything()) %>%
#         # mutate_if(function(x) inherits(x, "factor"), as.character)
#
# }

