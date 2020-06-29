
#' Wrapper around base::load that preserves the value of any user-specified variables
#'
#' @param rda.path path to loadable file, passed to `base::load` via the `file` argument
#' @param ... strings or a character vector of object names in current envir that should be restored after the load
#'
#' @return nothing
#' @export
#'
#' @examples
#' #load_preserve("path_to_RData_file", "DO_WRITES")
#' #load_preserve("path_to_RData_file", c("adf.wide","batch_names","load_exclude","condition_groups","et.modify"))
#'
load_preserve <- function(rda.path, ...){

	to.preserve = c(...)
	npreserve = length(to.preserve)

	if(npreserve > 0){
		# ... make sure all objects to preserve exist in current environment
		stopifnot(sapply(to.preserve, exists))

		# ... create temporary environment and stash preserved objects there
		tmp_env = new.env(parent = emptyenv())
		for(obj in to.preserve){
			assign(obj, get(obj), envir = tmp_env)
			#assign(obj, -1, envir = tmp_env)
		}
	}

	# ... load all objects from `rda.path`
	load(rda.path, envir = .GlobalEnv)

	if(npreserve > 0){
		# ... restore pre-load objects
		for(obj in ls(tmp_env)){
			assign(obj, get(obj, envir = tmp_env), envir = .GlobalEnv)
		}

		# ... explicitly remove the environment (for testing)
		rm(list=ls(tmp_env), envir = tmp_env)
		rm(tmp_env)
	}
}


