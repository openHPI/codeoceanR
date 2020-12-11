#' @title Find next largest relative order
#' @description Internal function to find the next largest relative order of scripts.
#'         Only necessary on Mac OS, not Windows.
#ToDo: test if if(rt_is_OS("Linux")) files <- rev(files) in rt_add_opened_files can now be removed
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020
#' @seealso [rt_add_opened_files()]
#' @keywords internal
#'
#' @param dir folder like ".Rproj.user/CONTEXT_ID/sources/per/t/"
#'
rt_nextlargestorder <- function(dir=".")
{
fn <- dir(dir,full.names=TRUE)[!grepl("contents",dir(dir))]
if(length(fn)==0) return(1)

o_vals <- sapply(fn, function(ff){
f <- readLines(ff, warn=FALSE)
f <- grep("relative_order", f, value=TRUE)
if(length(f)==0) return(0)
f <- strsplit(f, "[:|,]")[[1]][2]
f <- as.numeric(f)
f
})

max(o_vals) + 1
}
