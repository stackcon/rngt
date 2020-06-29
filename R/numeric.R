

#' Check if values are approximately zero
#'
#' Based on [this](https://www.rdocumentation.org/packages/bazar/versions/1.0.11/topics/almost.zero) implementation
#'
#' @param x vector of values to check whether near-zero
#' @param tolerance machine tolerance. related to numerical precision of the machine
#'
#' @return
#' @export
#'
approx.zero <- function(x, tolerance = sqrt(.Machine$double.eps)) {

	(x < tolerance) & (x > -tolerance)
}


