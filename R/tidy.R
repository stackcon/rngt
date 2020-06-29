####################################################################################################
#
# Functions that extend or are otherwise related to dplyr / tidyverse
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Split column values by a delimiter, then expand them into new rows
#'
#' @param indf input data.frame
#' @param .var the column in `indf` to split. should be character-type
#' @param delim the string delimiter to split on
#'
#' @return A data frame where data frae, `indf`, where the strings in  column, `.var`, have been split by delimiter, `delim`
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_at vars
#' @importFrom rlang enquo
#' @importFrom tidyr unnest
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(tidyr)
#' iris %>%
#' mutate_at(vars(Species), as.character) %>%
#' expand_on(Species, "o") %>%
#' select(Species) %>%
#' table
expand_on <- function(indf, .var, delim=";"){
	tmpvar <- enquo(.var)
	indf %>%
		mutate_at( vars(!!tmpvar), strsplit, split=delim, fixed=T) %>%
		unnest(!!tmpvar)
}
