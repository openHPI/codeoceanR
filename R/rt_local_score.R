#' @title locally score complete exercise
#' @description Run test script for entire exercise without server access.
#'              Intended for teacher / trainer use only.
#'              Requires *tests.R script to be present (hidden on CodeOcean and not downloaded in zip folder).
#' @return Vector with number of total and passed tests, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_score] for students. [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @importFrom berryFunctions checkFile normalizePathCP
#' @export
#'
#' @param tfile Name of *tests.R file to be run.
#'              Or name of exercise file, will be changed according to MOOC system.
#'              DEFAULT: NULL (will be obtained from currently open File in Rstudio)
#' @param check_unsaved Check if any files are unsaved? Requires dir to be in an .Rproj.
#'            Runs [rt_check_for_unsaved_files()]. DEFAULT: TRUE
#'
rt_local_score <- function(tfile=NULL, check_unsaved=TRUE)
{
# Obtain test file for currently selected document:
if(is.null(tfile)) tfile <- rstudioapi::documentPath()
if(!endsWith(tfile, "tests.R"))
  {
	callfile <- basename(tfile)
	tfile <- sub(".R$","_tests.R", sub("_[0-9]*\\.R$",".R",tfile))
  } else callfile <- NULL
#tfile <- c("a1.R","a_1.R","a_03_3_lis.R","a_03_4_arr_1.R","a_03_4_arr_2.R")#, "a_03_4_arr_tests.R")
# aufgabe1.R                                       -> aufgabe1_tests.R
# aufgabe_1.R                                      -> aufgabe_tests.R
# aufgabe_03_3_listen.R                            -> aufgabe_03_3_listen_tests.R
# aufgabe_03_4_arrays_1.R      -> \
# aufgabe_03_4_arrays_2.R      ->  all 3:  aufgabe_03_4_arrays_tests.R
# aufgabe_03_4_arrays_tests.R  -> /

message("-- running rt_local_score on ", tfile, if(!is.null(callfile)) paste0(", called from ", callfile))

berryFunctions::checkFile(tfile)
if(!grepl("tests\\.R$", tfile)) stop("tfile must end in *tests.R, but does not. ", tfile)

# Stop if files are changed but not saved:
if(check_unsaved) rt_check_for_unsaved_files(dirname(tfile), warnonly=TRUE)

# Actually run tests:
source(tfile, local=parent.frame())
return(invisible(NULL))
}
