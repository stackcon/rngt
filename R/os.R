####################################################################################################
#
# Functions for working with operating and file systems
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Return simplified operating system name
#'
#' @return
#' @export
#'
#' @examples
get_os <- function(){

	sysinf <- Sys.info()
	if (!is.null(sysinf)){
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "osx"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "osx"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}



#' Working with filenames
#'
#' Return the part of the file before the final period (i.e., the file extension).  The file does not have to exist
#'
#' @param filename a string representing a filename
#'
#' @return
#' @export
#'
#' @examples
file_prefix <- function(filename)
{

	ftmp = strsplit(filename,"\\.")[[1]]
	if(length(ftmp) == 1)
		return (ftmp)

	fpre = paste(head(ftmp,-1),collapse='.')
	fpre
}


#' @describeIn file_prefix Return the part of the file before the final period (i.e., the file extension).  The file does not have to exist
#' @export
file_ext <- function(filename)
{
	ftmp = strsplit(filename,"\\.")[[1]]
	if(length(ftmp) == 1)
		return ("")

	fext = tail(ftmp,1)
	fext
}







