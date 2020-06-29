####################################################################################################
#
# Functions related to indexing vectors and matrices
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Convert a vector of integers in to a matrix of ranges
#'
#' A function which converts a one-dimensional vector into a 2-column matrix of pairwise ranges that represent the range of numbers between subsequent values
#'
#' @param vec A vector of integers
#' @param with.max (optional) A number to use as the end point of the last range
#'
#' @return
#' @export
#'
#' @examples
vec2range <- function(vec, with.max=NULL){
	stopifnot(is.numeric(vec))
	out = cbind(
		head(vec,-1),
		(tail(vec,-1)-1)
	)
	if(!is.null(with.max)){
		out = rbind(out, c( tail(vec,1), with.max) )
	}
	out
}

#####

#' Expanded which() functions
#'
#'  Function that returns the indices of the top N observations
# NB -> which.topn(...,n=1) is equivalent to which.max
#'
#' @param vect a vector
#' @param n number of elements to return
#'
#' @return Returns the positions of top or bottom `n` elements of a vector
#' @export
#'
#' @examples
#' which.top(1:10, 7)
#' which.bottom(1:10, 3)
which.top <- function(vect, n = 5){
	thresh = min(head(sort(vect,decreasing=TRUE),n))
	which(vect >= thresh)
}

#' @describeIn which.top return the indices of NA values
#' @export
which.na <- function(vect) which(is.na(vect))


#' @describeIn which.top return the indices of the N smallest elements
#' @export
which.bottom <- function(vect, n = 5){
	thresh = max(head(sort(vect,decreasing=FALSE), n))
	which(vect <= thresh)
}


#' Return logical vector indicating location of low-incidence values in, x
#'
#' @param x Input vector
#' @param min.count the threshold number of occurrences
#' @param min.freq if set, then use frequency threshold instead of count
#' @param use.median Set minimum count to the median occurence in x
#' @param ... arguments passed to `low_incidence`
#'
#' @return
#' @export
#'
#' @examples
#' tmprnd = round(iris$Sepal.Length)
#' table(tmprnd, low_incidence(tmprnd, min.count = 5))
low_incidence <- function(x, min.count=1, min.freq, use.median = FALSE) {

	retval = NULL

	if(use.median) {
		min.count = median(tmptab)
		tmpinds = which(tmptab < min.count)
	}

	tmptab = table(x)

	# extract table indices with counts below min.count
	tmpinds = which(tmptab <= min.count)

	# ... add indices below the minimum frequency, if the latter was specified
	if(!missing(min.freq)){
		tmpfreq = tmptab / sum(tmptab)
		tmpinds = union(tmpinds, which(tmpfreq <= min.freq))
	}

	# return logical vector indicating the low-incidence values of x
	if(length(tmpinds)) {
		tmpvals = names(tmptab)
		retval = (x %in% tmpvals[tmpinds])
	}
	retval
}

#' @describeIn low_incidence an alias for `low_incidence`
#' @export
are_few <- function(...){
	low_incidence(...)
}

#' @describeIn low_incidence return the indices of low incidence values
#' @export
which_low <- function(...){
	which(low_incidence(...))
}


#' List duplicate values
#'
#' @param vect A vector that potentially contains duplicates
#' @param ... arguments passed to `get_duplicated`
#'
#' @return the names of duplicated values
#' @export
#' @importFrom methods as
#'
#' @examples
#' get_duplicated( c(1:10, 1:5) )
#' get_duplicated( c(1:10) )
get_duplicated <- function(vect) {
	output=c()
	junk = table(vect)
	if(any(junk>1))
		output = as( names(junk)[which(junk>1)], typeof(vect) )
	output
}

#' @describeIn get_duplicated check if any are duplicated
#' @export
has_duplicated <- function(...) length(get_duplicated(...)) > 0

#' @describeIn get_duplicated return indices of all values that are not unique in `vect`
#' @export
which.duplicated <- function(vect){
	dupvalues = get_duplicated(vect)
	which(vect %in% dupvalues)
}




