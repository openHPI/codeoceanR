#' @title Try to set contextIdentifier from .Rproj.user ID
#' @description Try to find the User-specific 8 digit hexadecimal id from .Rproj.user ID.
#'              Then save it to a file so that it can later be found by [rt_get_context_id()].
#'              If successful, it will be saved in "~/.rstudio_context_id_by_codeoceanR.txt".
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso [rt_get_context_id()]
#' @export
#' @param dir Path to some folder containing a .Rproj file
#'            and hence a (hidden) ".Rproj.user" folder. DEFAULT: "."
#'
rt_set_context_id <- function(dir=".")
{
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
ids <- dir(paste0(dir,"/.Rproj.user"), pattern="^[a-zA-Z0-9]{8}$")
ids <- ids[ids!="5A57A303"] # exclude failure output from rt_get_context_id
if(length(ids)==0) stop("Could not find any context ID in dir ", dir)
if(length(ids)!=1) warning("Found ", length(ids), " IDs in dir ", dir, "\nProceeding with ", ids[1])
ids <- ids[1]
cat(paste0('contextIdentifier="',ids,'"'), file="~/.rstudio_context_id_by_codeoceanR.txt")
return(ids)
}
