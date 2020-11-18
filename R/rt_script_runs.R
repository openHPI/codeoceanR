#' @title test whether script can be run
#' @description test whether script can be run
#' @return Logical. No [rt_warn] message issued, that already happens in [rt_run_script]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_run_script], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param scriptobject Charstring output from [rt_run_script] or [rt_select_script_section].
#'                     Will be checked with \code{!\link{isFALSE}()}.
#'
rt_script_runs <- function(scriptobject){
  !isFALSE(scriptobject)
}
