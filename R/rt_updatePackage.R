#' @title Update codeoceanR package to latest development version
#' @description Update codeoceanR to the latest development version on github, if necessary.
#'         If the version number or date is larger on github,
#'         [remotes::install_github()] will be called.
#' @return data.frame with version information
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2019 (rdwd), Oct 2020 (codeoceanR)
#' @seealso [remotes::install_github()]
#' @keywords file
#' @importFrom utils packageDescription download.file
#' @export
#' @examples
#' # rt_updatePackage()
#'
#' @param pack     Name of (already installed) package. DEFAULT: "codeoceanR "
#' @param user     Github username. repo will then be user/pack. DEFAULT: "openHPI"
#' @param quiet    Suppress version messages and `remotes::install` output?
#'                 DEFAULT: FALSE
#' @param \dots    Further arguments passed to [remotes::install_github()]
#'
rt_updatePackage <- function(
pack="codeoceanR ",
user="openHPI",
quiet=FALSE,
...
)
{
# installed date/version:
Vinst <- suppressWarnings(utils::packageDescription(pack)[c("Date","Version")])
repo <- paste0(user,"/",pack)
# date/version in source code
url <- paste0("https://raw.githubusercontent.com/",repo,"/master/DESCRIPTION")
tf <- tempfile("DESCRIPTION")
download.file(url, tf, quiet=TRUE)
Vsrc <- read.dcf(file=tf, fields=c("Date","Version"))
Vsrc <- split(unname(Vsrc),colnames(Vsrc)) # transform matrix to list
output <- data.frame(Version=c(Vinst$Version, Vsrc$Version),
                        Date=c(Vinst$Date,    Vsrc$Date))
rownames(output) <- paste0(pack,"_",c("Locally_installed", "Github_latest"))
# install if outdated:
doinst <-  Vsrc$Version > Vinst$Version   |   Vsrc$Date > Vinst$Date
if(!doinst)
{
if(!quiet) message(pack, " is up to date, compared to github.com/",repo,
         ". Version ", Vsrc$Version, " (", Vsrc$Date,")")
return(invisible(output))
}
if(!quiet) message(pack, " local version ", Vinst$Version, " (", Vinst$Date,
        ") is outdated.\nInstalling development version ",
        Vsrc$Version, " (", Vsrc$Date,") from github.com/",repo)
# check availability of remotes:
if(!requireNamespace("remotes", quietly=TRUE))
	stop("To use rt_updatePackage, please first install remotes :    install.packages('remotes')", call.=FALSE)
# unload:
if(!quiet) message("First unloading ",pack," so it can be installed by remotes::install_github.")
try(detach(paste0("package:",pack), character.only=TRUE, unload=TRUE), silent=TRUE)
# actually install
remotes::install_github(repo=repo, quiet=quiet, ...)
if(!quiet) message("Please re-load ",pack," now.  library(",pack,")  should do.")
return(invisible(output))
}
