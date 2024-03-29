#' @title Test whether object contains certain value(s)
#' @return Logical: TRUE / FALSE depending on whether `object` contains `value`.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020 + 2021
#' @keywords error test
#' @seealso [rt_has_value], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @examples
#' rt_contains(1:7, 6)
#' rt_contains(1:7, 8)
#' rt_contains(1:7, 5:6)
#' rt_contains(1:7, 5:9)
#' stri <- "some char string"
#' rt_contains(stri, "arstring")
#' rt_contains(stri, "arstring", ignore_space=FALSE)
#' rt_contains(stri, c("some", "char", "STRING"))
#'
#' @param object       Object to be tested. Just the name, not a character string.
#' @param value        value(s) that should be in `object`, can be char / other.
#' @param msgval       Value(s) to be messaged in the warning. Must be same length as value!
#'                     Enables testing with regex but displaying a wanted solution.
#'                     Remember to set fixed=FALSE as well. DEFAULT: `value`
#' @param name         [rt_warn] name. DEFAULT: deparse(substitute(obj))
#' @param qmark        Include ' marks around `name`? DEFAULT: TRUE
#' @param fixed        Fixed match in [grepl]? DEFAULT: TRUE
#' @param remcom       Remove comments from script before checking presence?
#'                     Experimental feature, uses [parse()].
#'                     DEFAULT: NA (yes, if object has length >1  and `value`
#'                     is a character string)
#' @param ignore_space Remove spaces before comparison? DEFAULT: TRUE
#' @param ignore_quote Replace `'` with `"` before comparison? DEFAULT: TRUE
#'
rt_contains <- function(
object,
value,
msgval=value,
name=deparse(substitute(object)),
qmark=TRUE,
fixed=TRUE,
remcom=NA,
ignore_space=TRUE,
ignore_quote=TRUE
){
force(name)
if(name=="code") qmark <- FALSE
pn <- if(qmark) paste0("'", name, "'") else name
value2 <- value # to keep 'value' as is for warning message
if(is.character(value))
  {
	if(is.na(remcom)) remcom <- length(object) > 1
	if(remcom){
		object <- try(as.character(parse(text=object)), silent=TRUE)
		if(inherits(object, "try-error")) return(rt_warn(
			pn, en=" raises error: ", de=" erzeugt Fehler: ", attr(object,"condition")$message))
	  }
  if(ignore_space) {
    object <- gsub("\\s", "", object)
  	value2 <- gsub("\\s", "", value2)
  	}
  if(ignore_quote) {
    object <- gsub("'", '"', object)
  	value2 <- gsub("'", '"', value2)
  	}
  }

for(i in seq_along(value))
  {
	v <- value2[i]
	succ <- if(is.character(v)) any(grepl(v, object, fixed=fixed)) else v %in% object
  if(!succ) return(rt_warn(pn, en=" should contain '",de=" sollte '",msgval[i], en="'.",de="' enthalten."))
  }

# pass:
TRUE
}
