#' @title Caret two class summary
#' @description A modification of the \pkg{caret} function \link[caret]{twoClassSummary}
#'   which allows the use of Gini as a training metric.
#' @details Here's is an example of how you would use it:
#'
#' \preformatted{
#' train(
#'     trainX,
#'     trainY,
#'     method = "glm",
#'     family = "binomial",
#'     metrix = "Gini"
#'     trControl = trainControl(
#'         classProbs = TRUE,
#'         summaryFunction = two_class_summary
#'     )
#' )
#' }
#'
#' @param data a data frame or matrix with columns \code{obs} and \code{pred} for the
#'   observed and predicted outcomes, in addition to columns giving the predicted
#'   probabilities for each class (the \code{classProbs} argument in
#'   \link[caret]{trainControl} needs to be set to \code{TRUE}).
#' @param lev a character vector of factors levels for the response. In regression cases,
#'   this would be \code{NULL}.
#' @param model a character string for the model name (as taken form the method argument
#'   of \link[caret]{train}).
#' @export

two_class_summary = function(data, lev = NULL, model = NULL){
    out = caret::twoClassSummary(data, lev = lev, model = model)
    out[["Gini"]] = 100 * (2 * out[["ROC"]] - 1)
    out
}