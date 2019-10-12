####################################################################################################
#
# Functions related to type conversions
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Convert number to bit vector
#'
#' @param number the number to convert to a binary vector
#' @param noBits the final length of the binary vector.  If missing, return the full (native) bit vector
#'
#' @return a vector of 0's and 1's
#' @export
#'
#' @examples
number2binary = function(number, noBits) {

	binary_vector = rev(as.numeric(intToBits(number)))
	if(missing(noBits)) {
		return(binary_vector)
	} else {
		binary_vector[-(1:(length(binary_vector) - noBits))]
	}
}


#' Convert R color string and output the color as hex
#'
#' @param xx The R color string to convert
#' @param transval transparency (alpha) value, as 2-character hex string.
#'
#' @return A hex string representing the color and transparency
#' @export
#'
#' @examples col2hex("red", "2F")
col2hex = function(xx,transval = NULL){

	outval = paste(c("#",sprintf("%02X",col2rgb(xx,alpha=T)[,1])),collapse="")
	if(!is.null(transval)){
		substring(outval,nchar(outval)-1,nchar(outval)) <- transval
	}
	outval
}


# convert raw to integers
#|best|# sapply(test, as.integer)
#|acceptable|# sapply(test, function(xx) as.integer( suppressMessages(dput(xx)) )) 


