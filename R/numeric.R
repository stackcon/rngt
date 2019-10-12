

# modeled off of https://www.rdocumentation.org/packages/bazar/versions/1.0.11/topics/almost.zero
approx.zero <- function(x, tolerance = sqrt(.Machine$double.eps)) {
	(x < tolerance) & (x > -tolerance)
}
