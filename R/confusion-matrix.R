
#' @name confusion
#' @rdname confusion
#' @title Confusion matrix metrics
#' @description Helper functions for calculating confusion matrix metrics.
#' @param tp n true positive (numeric) 
#' @param fp n false positive (numeric)
#' @param fn n false negative (numeric)
#' @param tn n true negative (numeric)
#' @details
#' TPR = TP / (TP + FN) = Recall = Sensitivity
#' 
#' FNR = FN / (TP + FN)
#' 
#' TNR = TN / (TN + FP) = Specificity
#' 
#' FPR = FP / (TN + FP)
#' 
#' Support = TP + FN
#' 
#' Prevalence = (TP + FN) / (TP + FP + FN + TN)
#' 
#' Accuracy = (TP + TN) / (TP + FP + FN + TN)
#' 
#' Precision = TP / (TP + FP)
#' 
#' Lift = Precision / Prevalence
#' 
#' @examples
#' tp = 87
#' fn = 13
#' tn = 76
#' fp = 32
#' tpr(tp, fp, fn, tn)
#' tnr(tp, fp, fn, tn)
#' fpr(tp, fp, fn, tn)
#' fnr(tp, fp, fn, tn)
#' sensitivity(tp, fp, fn, tn)
#' specificity(tp, fp, fn, tn)
#' precision(tp, fp, fn, tn)
#' recall(tp, fp, fn, tn)
#' prevalence(tp, fp, fn, tn)
#' accuracy(tp, fp, fn, tn)
#' support(tp, fp, fn, tn)
#' f1(tp, fp, fn, tn)
NULL

#' @rdname confusion
#' @export
tpr = function(tp, fp, fn, tn){
  tp / (tp + fn)
}

#' @rdname confusion
#' @export
tnr = function(tp, fp, fn, tn){
  tn / (tn + fp)
}

#' @rdname confusion
#' @export
fnr = function(tp, fp, fn, tn){
  fn / (tp + fn)
}

#' @rdname confusion
#' @export
fpr = function(tp, fp, fn, tn){
  fp / (tn + fp)
}

#' @rdname confusion
#' @export
recall = function(tp, fp, fn, tn){
  tpr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @export
sensitivity = function(tp, fp, fn, tn){
  tpr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @export
specificity = function(tp, fp, fn, tn){
  tnr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @export
support = function(tp, fp, fn, tn){
  tp + fn
}

#' @rdname confusion
#' @export
prevalence = function(tp, fp, fn, tn){
  (tp + fn) / (tp + fp + fn + tn)
}

#' @rdname confusion
#' @export
accuracy = function(tp, fp, fn, tn){
  (tp + tn) / (tp + fp + fn + tn)
}

#' @rdname confusion
#' @export
precision = function(tp, fp, fn, tn){
  tp / (tp + fp)
}

#' @rdname confusion
#' @export
lift = function(tp, fp, fn, tn){
  precision(tp, fp, fn, tn) / prevalence(tp, fp, fn, tn)
}

#' @rdname confusion
#' @export
f1 = function(tp, fp, fn, tn){
  (2 * tp) / (2 * tp + fp + fn)
}
