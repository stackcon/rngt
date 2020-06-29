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

#' @describeIn na.freq Function that returns the number frequency of missing values for each row
row.missing <- function(dd){
	apply(dd,1, na.freq)
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
fqtable <- function(vect, scaling = 1, hightolow = TRUE, ... ) {

	tmptab = table(vect, ...)

	tmpdf = data.frame(
		value = as(names(tmptab), typeof(vect)),
		n = unname(as.integer(tmptab)),
		freq = as.numeric((tmptab / sum(tmptab)) * scaling)
	)

	if(hightolow)
		tmpdf = tmpdf[order(tmpdf$freq, decreasing = TRUE),]

	tmpdf
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
	if(is.data.frame(inp)) {
		id <- sapply(inp, is.numeric)
		if(any(!id)){
			retval = lapply(inp[!id],.chknum)
		}
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


#' Assign ID's to rows based on their valuees
#'
#' @param df the data frame to operate on
#'
#' @return
#' @export
#'
#' @examples
label_duplicates <- function(rcfilt, stopmax=Inf, verbose=FALSE) {

	npairs = choose(nrow(rcfilt), 2)
	if(npairs > stopmax){
		stop(sprintf("Stopping because number of pairs to search (%d) is higher than stopmax (%d)", npairs, stopmax))
	}

	inds = combn(nrow(rcfilt), 2)
	vsame = rep(NA, ncol(inds))
	for(i in seq(ncol(inds))){
	    if(verbose && i %% 1000 == 0) print(i)
	    v1 = unlist(rcfilt[inds[1,i],])
	    v2 = unlist(rcfilt[inds[2,i],])
	    vsame[i] = isTRUE(
	        all.equal(v1, v2, check.attributes=FALSE,
	                  tolerance = (.Machine$double.eps)^(1/2))
	    )
	}

	# ... connect identical entries and their communities
	vyes = t(inds[1:2, vsame])
	vgr = igraph::graph_from_edgelist(vyes, directed=FALSE)
	vclust = igraph::components(vgr)

	# return
	return(vclust$membership)
}


