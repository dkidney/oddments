
#' @rdname rotate_coords
#' @title Rotate a set of points by a given angle
#' @description TODO
#' @details TODO
#' @param x a 2-column matrix of coordinates to rotate
#' @param angle angle of rotation
#' @param axis axis of rotation
#' @param clockwise logical scalar indicating whether the direction of rotation should be
#'   clockwise or anti-clockwise
#' @param units angle units
#' @return TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @example inst/examples/examples-rotate_coords.R

NULL

#' @rdname rotate_coords
#' @name rotate_coords
#' @export
rotate_coords <- function(x, angle, axis = c(0, 0), clockwise = TRUE,
                          units = c("radians", "degrees")) {
  units <- match.arg(units)
  # check coords
  if (!inherits(x, c("matrix", "data.frame", "array")) || ncol(x) != 2) {
    stop("'coords' must be a 2-column matrix")
  }
  x <- as.matrix(x)
  if (units == "degrees") {
    angle <- angle * (2 * pi) / 360
  }
  # if anti-clockwise, calculate the clockwise angle
  if (!clockwise) angle <- 2 * pi - angle
  # rotaion matrix
  R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), ncol = 2)
  # centre the coords by subtracting the axis coordinates from each coordinate in x
  for (i in 1:2) {
    x[, i] <- x[, i] - axis[i]
  }
  # rotate the centred coordinates
  rotated <- x %*% R
  # add the axis coordinates to the rotated coordinates
  for (i in 1:2) {
    rotated[, i] <- rotated[, i] + axis[i]
  }
  dimnames(rotated) <- dimnames(x)
  return(rotated)
}
