####################################################################################################
#
# Functions for working with strings
#
# @author ConradStack <conrad.stack@gmail.com>
#

.REGEX_parenthesis = "\\([^\\(\\)]{0,}\\)"
.REGEX_bracketed = "\\[[^\\[\\]]{0,}\\]"
.REGEX_curlies = "\\{[^\\{\\}]{0,}\\}"


#' Reduce consecutive, internal whitespace characters (not leading or trailing) to a single whitespace character
#'
#' @param x A chacter vector
#' @param ... additional arguments passed to `regex` function
#'
#' @return
#' @export
#' @importFrom stringr str_remove_all
#'
#' @examples
#' reducews("CCN      51")
#' @seealso [stringr:::str_squish()] which does essentially the same thing
reducews <- function(x){
	gsub("(\\s)\\s+","\\1",x)
}

# Remove different blocks that are delineated by parentheses, square brackets, or curly brackets

#' @describeIn reducews remove text within parentheses
#' @export
rm.paren <- function(x, ...) str_remove_all(x, stringr::regex(.REGEX_parenthesis, ...) )

#' @describeIn reducews remove text within square braces
#' @export
rm.brack <- function(x, ...) str_remove_all(x, stringr::regex(.REGEX_bracketed, ...))

#' @describeIn reducews remove text within curly braces
#' @export
rm.curly <- function(x, ...) str_remove_all(x, stringr::regex(.REGEX_curlies, ...))

#' @describeIn reducews remove text within parentheses, or square or curly braces
#' @export
remove.blocks <- function(x, ...) {
	rm.paren(rm.brack(rm.curly(x, ...), ...), ...)  # remove all three
}

#' @describeIn reducews extract parenthetical expressions
#' @export
extract.paren <- function(x, ...) str_extract_all(x, stringr::regex(.REGEX_parenthesis, ...), simplify = F )

#' @describeIn reducews extract parenthetical expressions
#' @export
extract.brack <- function(x, ...) str_extract_all(x, stringr::regex(.REGEX_bracketed, ...), simplify = F )

#' @describeIn reducews extract parenthetical expressions
#' @export
extract.curly <- function(x, ...) str_extract_all(x, stringr::regex(.REGEX_curlies, ...), simplify = F )


#' Check for any non-ASCII characters
#'
#' @param x vector of strings
#' @param ... additional arguments passed to grepl
#'
#' @return a logical vector the same length as x
#' @export
#'
has_nonascii <- function(x, ...) {
	grepl("[^\\x00-\\x7F]", x , perl = TRUE, useBytes = TRUE, ...)
}

#' @describeIn has_nonascii (DEPRECATED) alias for has_nonascii, included for back-compatibility
#' @export
any_nonascii <- function(...) has_nonascii(...)


# Method that extracts (generally) parenthetical information from a name
# N.B. -> the default regular expressions assume a single annotation block is present.  They also assume that tree names have the following structure: <Letter code>.*<Number code>
#' Extract (generally) parenthetical annotations from within a string
#'
#' @param x string to extract annotations from
#' @param opening character that indicates the start of an annotation block
#' @param closing character that indicates the end of an annotation block
#' @param auto.fix if TRUE, then additional closing characters are checked when an open block has no closing character.
#' @param other.closing other closing characters to look for.  only used when auto.fix=TRUE
#'
#' @return
#' @export
#'
#' @examples  get_anno("this is a (test)")
get_anno <- function(x,
							opening="(",
							closing=")",
							auto.fix=FALSE,
							other.closing=c("}","\\]","\\)")) {

	.GENERAL = "^([^\\%1$s]*)\\%1$s([^\\%2$s]*)\\%2$s(.*)$"
	.USE = sprintf(.GENERAL,opening,closing)
	retval = c(orig=x, before="", paren="", after="")
	if(!is.na(x) && grepl(sprintf("\\%s",opening),x) ){
		# contains start of parentheses
		has.closing = grepl(sprintf("\\%s",closing),x)
		if(!has.closing && !auto.fix){
			warning(sprintf("Name contains an opening parentheses, but not a closing one: %s",x))
		} else {

			if(!has.closing){
				has.others = sapply(other.closing,grepl,x)
				if(!any(has.others)){
					stop(sprintf("Name could not be automatically fixed: %s",x))
				} else {
					.USE = sprintf(.GENERAL,opening,other.closing[which(has.others)[1]])
				}
			}

			before = trimws(sub(.USE,"\\1",x,perl=TRUE))
			paren =  trimws(sub(.USE,"\\2",x,perl=TRUE))
			after =  trimws(sub(.USE,"\\3",x,perl=TRUE))
			retval[2:4] <- c(before,paren,after)
		}
	}
	retval
}


#' Function that chops a strings into tokens based on one or two delimiters
#'
#' @param str A string
#' @param first.lev Delimiter to chop string on first
#' @param second.lev Delimiter to chop up results from first one
#'
#' @return
#' @export
#'
ngt_tokenize <- function(str, first.lev = ",", second.lev=NA)
{
	# Note - there are probably way better functions and data structures to handle this sort of thing in R
	#
	toks = strsplit(str, first.lev )
	if(!is.na(second.lev))
		toks = lapply(toks, strsplit, split=second.lev )
	toks
}



