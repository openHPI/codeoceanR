#' @title Test function call for certain arguments
#' @description Test if a function call in stundent's code has certain arguments.
#' If argument value is a charstring, test with `rt_has_argument(code, "arg", '"value"')`.\cr
#' Use double backslashes as in `rt_has_argument(code, "arg", '"value\\U{00B0}"')`
#' for Unicode etc. \cr
#' Attenton: if the user script contains line breaks and is read with
#' `rt_script_section(collapse=";")`, `rt_has_argument` signals a failure.
# ToDo: handle escaping slashes etc better, see rt_score
#' @return Logical: TRUE / FALSE depending on whether condition is met
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_script_section], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#' @importFrom utils getParseData
#' @export
#'
#' @param code  Charstring with code, normally from [rt_script_section]
#' @param arg   Charstring with a single argument name to be tested
#' @param value Value the argument should have. Use `'escaped "quotation" marks'`.
#'              Can be left NULL if only the presence of the argument is tested, not its value.
#' @param ignore_space Remove spaces before comparison? DEFAULT: TRUE
#' @param notsub Arguments that should not be treated as a sub-symbol.
#'               E.g. for `value="mean(x,na.rm=TRUE)"`, `na.rm` in `code`
#'               is parsed as a new sub-level and test would fail.
#'               Avoid this manually with `notsub="na.rm"`.
#'               Do not set ignore_space to FALSE when using this.
#'               DEFAULT: NULL (ignored)
#'
rt_has_argument <- function(code, arg, value=NULL, ignore_space=TRUE, notsub=NULL){
  if(!any(grepl(paste0("\\<",arg,"\\>"), code))) # \<arg\> as standalone word
    return(rt_warn("The code does not contain the argument '",arg,"'."))
  if(is.null(value)) return(TRUE)

  cd <- getParseData(parse(text=code, keep.source=TRUE))
  cd <- cd[cd$terminal,c("parent", "token","text")]
  if(!is.null(notsub)) cd[cd$text %in% notsub,"token"] <- "originally_SYMBOL_SUB"

  # start:
  l1 <- which(cd$token=="SYMBOL_SUB" & cd$text==arg)
  # This should always be one value. Messaging just in case...
  if(length(l1)!=1) return(rt_warn("Could not extract start of argument '",arg,
  																 "' from code tree (found ",length(l1)," instances)."))
  # end:
  to_add <- data.frame(parent=0, token="SYMBOL_SUB", text="dummy")
  where <- max(which(cd$parent==cd$parent[l1]))
  cd <- berryFunctions::insertRows(cd, where+1, to_add) # for last argument
  l2 <- which(cd$token=="SYMBOL_SUB")
  l2 <- l2[l2>l1][1]

  # selection:
  cd <- paste0(cd$text[(l1+2):(l2-2)],collapse="")
  if(value=="\t") value <- '"\\t"' # hard-coded for read/write.table tasks
  if(value=="\n") value <- '"\\n"'
  if(value=="\\") value <- '"\\\\"'
  cd2 <- gsub("'", "\"", cd)
  value2 <- value # to keep 'value' as is for warning message
  if(ignore_space)
    {
    cd2 <- gsub("\\s", "", cd2)
    value2 <- gsub("\\s", "", value2)
    }
  if(value2==cd2) TRUE else {rt_warn("The ", arg, " argument should be '",value,"', not '",cd,"'."); FALSE}
}
