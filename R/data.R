####################################################################################################
#
# Functions related to data processing and investigation
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Return unique values
#'
#' Function that returns the number of unique values in a particular vector
#'
#' @param x A vector
#'
#' @return
#' @export
#'
#' @examples
n.unique <- function(x){
	length(unique(x))
}

#' Return the relative frequency of missing values
#'
#' @param vec A vector
#'
#' @return
#' @export
#'
#' @examples
na.freq <- function(vec) {
	length(which(is.na(vec))) / length(vec)
}

#' @describeIn na.freq Function that returns the number frequency of missing values for each column
col.missing <- function(dd){
	sapply(dd, na.freq)
}

#' Calculate table and return relative frequencies
#'
#' @param vect
#' @param scaling multiple frequencies by this scaling factor (e.g, 100 to get percentages)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fqtable <- function(vect, scaling = 1, ... ) {
	tmptab = table(vect, ...)
	data.frame(
		value = as(names(tmptab), typeof(vect)),
		freq = as.numeric((tmptab / sum(tmptab)) * scaling)
	)
}

####

#
#' Find and return values that look non-numeric
#'
#' @param inp An input vector (usually a character vector)
#' @param val passed to grep(value=val)
#' @param uvals Return only unique values?
#' @param nsamp The number of (down) samples to return.  For instances where inp is very large
#'
#' @return
#' @export
#'
#' @examples
check.numeric <- function(inp, val = TRUE , uvals = val, nsamp = NULL){
	reval = NULL
	.chknum <- function(xx){
		tmpout = grep("[^0-9\\.]", xx, value=val)
		if(uvals) tmpout <- unique(tmpout)
		tmpout
	}

	# check the values
	if(is.data.frame(inp) || is.matrix(inp)) {
		retval = apply(inp,2,.chknum)
	} else {
		retval = .chknum(inp)
	}

	retval
}



#' Convert all factors in a data.frame to strings
#'
#' @param df the data frame to operate on
#'
#' @return
#' @export
#'
#' @examples
unfactor <- function(df) {
	id <- sapply(df, is.factor)
	df[id] <- lapply(df[id], as.character)
	df
}




