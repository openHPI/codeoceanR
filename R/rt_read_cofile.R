#' @title Read .co file
#' @description Read .co file (from downloaded CO task).
#' @return List with elements 'token', 'url', 'files'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2021
#' @seealso \code{\link{rt_score}}
#' @keywords file
#' @importFrom berryFunctions normalizePathCP l2df
#' @param cofile Path of .co file, may be relative.
#'
rt_read_cofile <- function(cofile)
{
# check if dir is an exercise directory
dir <- berryFunctions::normalizePathCP(dirname(cofile))
if(!file.exists(cofile)) stop("You're not in a CodeOcean exercise directory. ",
					"The file '.co' does not exist. \nYou're at '", dir, "'\n",
					"Make sure you have run rt_create_task() and the 'zz_exercise*.Rproj' file has been opened.\n",
					"In Rstudio on the top right, the R Project symbol should show the exercise name.", call.=FALSE)

co <- readLines(cofile, warn=FALSE)
out <- list()
out$token <- co[1]
out$url   <- co[2]

co_files <- co[-(1:2)]
if(length(co_files)<1) stop("No filenames found to be submitted in file '",cofile,"'")
co_files <- berryFunctions::l2df(strsplit(co_files, "="))
colnames(co_files) <- c("name", "id")
out$files <- co_files

return(out)
}