
#' @rdname confusion
#' @name tpr
#' @title ???
#' @description ???
#' @param tp ???
#' @param fp ???
#' @param fn ???
#' @param tn ???
#' @details
#' TPR = TP / (TP + FN) = Recall = Sensitivity
#' FNR = FN / (TP + FN)
#' TNR = TN / (TN + FP) = Specificity
#' FPR = FP / (TN + FP)
#' Support = TP + FN
#' Prevalence = (TP + FN) / (TP + FP + FN + TN)
#' Accuracy = (TP + TN) / (TP + FP + FN + TN)
#' Precision = TP / (TP + FP)
#' Lift = Precision / Prevalence
#' @export
tpr = function(tp, fp, fn, tn){
    tp / (tp + fn)
}

#' @rdname confusion
#' @name fnr
#' @export
fnr = function(tp, fp, fn, tn){
    fn / (tp + fn)
}

#' @rdname confusion
#' @name tnr
#' @export
tnr = function(tp, fp, fn, tn){
    tn / (tn + fp)
}

#' @rdname confusion
#' @name fpr
#' @export
fpr = function(tp, fp, fn, tn){
    fp / (tn + fp)
}

#' @rdname confusion
#' @name recall
#' @export
recall = function(tp, fp, fn, tn){
    tpr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @name sensitivity
#' @export
sensitivity = function(tp, fp, fn, tn){
    tpr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @name specificity
#' @export
specificity = function(tp, fp, fn, tn){
    tnr(tp, fp, fn, tn)
}

#' @rdname confusion
#' @name support
#' @export
support = function(tp, fp, fn, tn){
    tp + fn
}

#' @rdname confusion
#' @name prevalence
#' @export
prevalence = function(tp, fp, fn, tn){
    (tp + fn) / (tp + fp + fn + tn)
}

#' @rdname confusion
#' @name accuracy
#' @export
accuracy = function(tp, fp, fn, tn){
    (tp + tn) / (tp + fp + fn + tn)
}

#' @rdname confusion
#' @name precision
#' @export
precision = function(tp, fp, fn, tn){
    tp / (tp + fp)
}

#' @rdname confusion
#' @name lift
#' @export
lift = function(tp, fp, fn, tn){
    precision(tp, fp, fn, tn) / prevalence(tp, fp, fn, tn)
}
