#' @title Determine or check for OS platform
#' @description Determine or check for OS platform.
#'              Possible are: Linux, Windows, Darwin (Mac), SunOS (Solaris)
#' @return OS name if `os=NULL`, TRUE/FALSE otherwise (see examples)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @export
#' @examples
#' rt_is_OS()
#' rt_is_OS("Linux")
#' rt_is_OS(c("windows","Linux")) # TRUE if one of those
#' rt_is_OS(c("Mac","Linux"))
#' rt_is_OS(c("Mac","windows"))
#'
#' @param os Path to (processed) exercise folder.

#'
rt_is_OS <- function(os=NULL)
{
sysos <- Sys.info()["sysname"] #
sysos <- unname(sysos)
if(!is.null(os)) tolower(sysos) %in% sub("mac", "darwin", tolower(os)) else sysos
}
