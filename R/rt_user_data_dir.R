#' @title Get directory for .Rproj.user folder
#' @description Internal function to find the project-specific path for .Rproj.user
#' @return path
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2025
#' @seealso [rt_add_opened_files()]
#' @keywords internal
#'
rt_user_data_dir <- function()
{
# https://github.com/rstudio/rstudio/pull/15140
pudir <- rstudioapi::readRStudioPreference("project_user_data_directory", "")
if(pudir=="") return("") # in rt_add_opened_files, use project-specific local hidden folder "/.Rproj.user"

existing <- dir(pudir, "^b0")
exinum <- gsub("[-a-z]","",existing)
if(length(exinum)<1) exinum <- 0
eximax <- max(as.numeric(exinum))
path <- paste0("b",berryFunctions::round0(eximax+1, pre=31))
# add minus symbols:
for(i in cumsum(c(8,5,5,5))) path <- paste0(substr(path,1,i),"-",substr(path,i+1,42))
if(nchar(path) != 36) stop("generated guid should have 32+4 symbols, not ", nchar(path),".")
out <- paste0(pudir,"/",path)
if(dir.exists(out)) stop("generated guid is not unique: ", out)
return(out)
}
