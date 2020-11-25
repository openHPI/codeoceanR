#' @title Test function call for certain arguments
#' @description Test if a function call in stundent's code has certain arguments.
#' If argument value is a charstring, test with `rt_has_argument(code, "arg", '"value"')`.\cr
#' Attenton: if the user script contains line breaks and is read with
#' `rt_select_script_section(collapse=TRUE)`, `rt_has_argument` signals a failure.
# ToDo: handle escaping slashes etc better, see rt_score
#' @return Logical: TRUE / FALSE depending on whether condition is met
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_select_script_section], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#' @importFrom utils getParseData
#' @export
#'
#' @param code  Charstring with code, normally from [rt_select_script_section]
#' @param arg   Charstring with a single argument name to be tested
#' @param value Value the argument should have. Use `'escaped "quotation" marks'`.
#'              Can be left NULL if only the presence of the argument is tested, not its value.
#'
rt_has_argument <- function(code, arg, value=NULL){
  if(!any(grepl(paste0("\\<",arg,"\\>"), code))) # \<arg\> as standalone word
    {rt_warn("Code does not contain argument '",arg,"'."); return(FALSE)}
  if(is.null(value)) return(TRUE)

  cd <- getParseData(parse(text=code, keep.source=TRUE))
  cd <- cd[cd$terminal,c("parent", "token","text")]

  # start:
  l1 <- which(cd$token=="SYMBOL_SUB" & cd$text==arg)
  # This should always be one value. Messaging just in case...
  if(length(l1)!=1) {rt_warn("Could not extract start of argument '",arg,"' from code tree (found ",length(l1)," instances)."); return(FALSE)}

  # end:
  to_add <- data.frame(parent=0, token="SYMBOL_SUB", text="dummy")
  where <- max(which(cd$parent==cd$parent[l1]))
  cd <- berryFunctions::insertRows(cd, where+1, to_add) # for last argument
  l2 <- which(cd$token=="SYMBOL_SUB")
  l2 <- l2[l2>l1][1]
  # This should always be one value. Messaging just in case...
  if(length(l2)!=1) {rt_warn("Could not extract end of argument '",arg,  "' from code tree (found ",length(l2)," instances)."); return(FALSE)}

  # selection:
  cd <- paste0(cd$text[(l1+2):(l2-2)],collapse="")
  if(value=="\t") value <- '"\\t"' # hard-coded for read/write.table tasks
  if(value=="\n") value <- '"\\n"'
  if(value=="\\") value <- '"\\\\"'
  cd <- gsub("'", "\"", cd)
  if(value==cd) TRUE else {rt_warn(arg, " argument should be '",value,"', but is '",cd,"'."); FALSE}
}
