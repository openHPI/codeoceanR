#' @title Update codeoceanR package to latest development version
#' @description Update codeoceanR to the latest development version on github, if necessary.
#'         If the version number or date is larger on github,
#'         [remotes::install_github()] will be called.
#' @return data.frame with version information
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2019 (rdwd), Oct 2020 (codeoceanR)
#' @seealso [remotes::install_github()], <https://github.com/openHPI/codeoceanR>
#' @keywords file
#' @importFrom utils packageDescription download.file compareVersion
#' @export
#' @examples
#' # codeoceanR::rt_updatePackage()
#' # codeoceanR::rt_update_package("berryFunctions", "brry", "master")
#'
#' @param pack     Name of (already installed) package. DEFAULT: "codeoceanR "
#' @param user     Github username. repo will then be user/pack. DEFAULT: "openHPI"
#' @param branchdesc Github branch with description file. For many packages, "master" is needed. DEFAULT: "main"
#' @param quiet    Suppress version messages output?  DEFAULT: FALSE
#' @param quietremotes  Suppress `remotes::install` output? DEFAULT: TRUE
#' @param force    Force installation, even if not outdated? DEFAULT: FALSE
#' @param ignoreInScripted Ignore this function in a scripted instance,
#'                 e.g. when R is called through  `Rscript` as it is on CodeOcean.
#'                 Determined with [interactive()].
#'                 DEFAULT: TRUE (so `rt_update_package` calls can safely be
#'                 in scripts submitted to CO through [rt_score()].)
#' @param \dots    Further arguments passed to [remotes::install_github()]
#'
rt_update_package <- function(
pack="codeoceanR",
user="openHPI",
branchdesc="main",
quiet=FALSE,
quietremotes=TRUE,
force=FALSE,
ignoreInScripted=TRUE,
...
)
{
if(ignoreInScripted) if(!interactive()) return("Not running 'rt_update_package' because R session is not interactive.")
# installed date/version:
Vinst <- suppressWarnings(utils::packageDescription(pack)[c("Date","Version")])
repo <- paste0(user,"/",pack)
# date/version in source code
url <- paste0("https://raw.githubusercontent.com/",repo,"/",branchdesc,"/DESCRIPTION")
tf <- tempfile("DESCRIPTION")
ee <- suppressWarnings(try(download.file(url, tf, quiet=TRUE), silent=TRUE))
if(inherits(ee, "try-error")) {warning("Download failed. ", ee); return(invisible(ee))}

Vsrc <- read.dcf(file=tf, fields=c("Date","Version"))
Vsrc <- split(unname(Vsrc),colnames(Vsrc)) # transform matrix to list
output <- data.frame(Version=c(Vinst$Version, Vsrc$Version),
                        Date=c(Vinst$Date,    Vsrc$Date))
rownames(output) <- paste0(pack,"_",c("Locally_installed", "Github_latest"))
if(anyNA(output$Date)) stop("Date field is missing, cannot be compared.")
# install if outdated:
doinst <-  compareVersion(Vsrc$Version, Vinst$Version)==1   |   Vsrc$Date > Vinst$Date
doinst <- doinst || force
if(!doinst)
{
if(!quiet) message(pack, " is up to date, compared to github.com/",repo,
         ". Version ", Vinst$Version, " (", Vinst$Date,")")
return(invisible(output))
}
# message installation process:
if(!quiet) message(pack, " local version ", Vinst$Version, " (", Vinst$Date,
        ") is outdated.\nInstalling development version ",
        Vsrc$Version, " (", Vsrc$Date,") from github.com/",repo, "\nThis may take a few seconds...")
# check availability of remotes:
if(!requireNamespace("remotes", quietly=TRUE))
	stop("To use rt_updatePackage, please first install remotes :    install.packages('remotes')", call.=FALSE)
# unload:
try(detach(paste0("package:",pack), character.only=TRUE, unload=TRUE), silent=TRUE)
# actually install
remotes::install_github(repo=repo, quiet=quietremotes, ...)
if(!quiet) message("Done!  Please re-load ",pack," now.  library(",pack,")  should do.")
return(invisible(output))
}
