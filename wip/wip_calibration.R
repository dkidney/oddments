
# @rdname calibration
# @name calibration
# @title Calibrate predictions
# @description Calibrate predictions using Platt scaling.
# @param y a vector of binary response values
# @param x a vector of predictions
# @export
# @example inst/examples/example-calibration.R

# @rdname calibration
# @name define_calibration
# @export
define_calibration = function(y, x){
    if(length(x) != length(y))
        stop("length(x) not equal to length(y)")
    stats::glm(y ~ x, "binomial", data.frame(x = x, y = y))
}

# @rdname calibration
# @name apply_calibration
# @param definition an object returned from \code{define_calibration} (or a suitable
#   \link[stats]{glm} model object)
# @export
apply_calibration = function(x, definition){
    if(!inherits(definition, 'glm'))
        stop("expecting a glm model object")
    cov_name = f_covs(definition$formula)
    if(length(cov_name) != 1)
        stop("expecting a model with a single predictor")
    newdata = data_frame(x = x)
    colnames(newdata) = cov_name
    preds = definition %>%
        stats::predict.glm(type = "response", newdata = newdata) %>%
        unname
    if(length(preds) == length(x))
        stop("something went wrong in predict.glm: too few predictions")
    preds
}
