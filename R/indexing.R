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

#


#####

#' Expanded which() functions
#'
#'  Function that returns the indices of the top N observations
# NB -> which.topn(...,n=1) is equivalent to which.max
#'
#' @param vect A numeric vector
#' @param n
#'
#' @return
#' @export
#'
#' @examples
which.topn <- function(vect, n){
	thresh = min(head(sort(vect,decreasing=TRUE),n))
	which(vect >= thresh)
}

#' @describeIn which.topn return the indices of NA values
which.na <- function(vect) which(is.na(vect))

#' @describeIn which.topn return the indices of the top 5 largest elements
which.top5 <- function(...)  which.topn(..., n=5)
#' @describeIn which.topn return the indices of the top 10 largest elements
which.top10 <- function(...)  which.topn(..., n=10)


#' @describeIn which.topn return the indices of the N smallest elements
which.botn <- function(vect, n){
	thresh = max(head(sort(vect,decreasing=FALSE), n))
	which(vect <= thresh)
}

#' @describeIn which.topn return the bottom 5 element
which.bot5 <- function(...)  which.botn(..., n=5)
#' @describeIn which.topn return the bottom 10 elements
which.bot10 <- function(...)  which.botn(..., n=10)


#' Return logical vector indicating location of low-incidence values in, x
#'
#' @param x Input vector
#' @param min.count the threshold number of occurrences
#' @param min.freq if set, then use frequency threshold instead of count
#' @param use.median Set minimum count to the median occurence in x
#'
#' @return
#' @export
#'
#' @examples
low.incidence <- function(x, min.count=1, min.freq, use.median = FALSE) {

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

#' @describeIn low.incidence return the indices of low incidence values
which.low <- function(...){
	which(low.incidence(...))
}

#' Identify duplicate values within a vector
#'
#' @param vect A vector that potentially contains duplicates
#'
#' @return the names of duplicated values
#' @export
#'
#' @examples
get.duplicated <- function(vect) {
	output=NULL
	junk = table(vect)
	if(any(junk>1))
		output = as( names(junk)[which(junk>1)], typeof(vect) )
	output
}

#' @describeIn get.dups
has.duplicated <- function(...) !is.null(get.duplicated(...))




