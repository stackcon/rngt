####################################################################################################
#
# Functions related to the local system and R environment
#
# @author ConradStack <conrad.stack@gmail.com>
#

# # Mimics 'clear' command from unix ... sort of
# makeActiveBinding(sym = "clear",
# 						fun = function(value){
# 							cat(rep("\n", 50))
# 						},
# 						env = .GlobalEnv
# )
#
# # pwd - print present working directory
# makeActiveBinding(sym = "pwd",
# 						fun = function(value){
# 							getwd()
# 						},
# 						env = .GlobalEnv
# )



#' Read data table from system clipboard
#'
#' @param ... arguments passed to read.table
#'
#' @return a data frame created from clipboard data
#' @export
#'
#' @examples
read.cb <- function(...) {

	ismac <- (get_os() == "osx")
	if (!ismac) read.table(file="clipboard", ...)
	else read.table(pipe("pbpaste"), ...)
}



