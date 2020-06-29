####################################################################################################
#
# Functions that extend or are otherwise related to dplyr / tidyverse
#
# @author ConradStack <conrad.stack@gmail.com>
#


#' Split column values by a delimiter, then expand them into new rows
#'
#' @param indf
#' @param .var
#'
#' @return
#' @export
#'
#' @examples
expand_on <- function(indf, .var){

	tmpvar <- enquo(.var)
	indf %>%
		mutate_at( vars(!!tmpvar), strsplit, split=";", fixed=T) %>%
		unnest(!!tmpvar)
}




