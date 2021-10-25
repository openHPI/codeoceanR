#' @title Test whether object contains certain value(s)
#' @return Logical: TRUE / FALSE depending on whether `object` contains `value`.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020 + 2021
#' @keywords error test
#' @seealso [rt_has_value], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @examples
#' rt_test_env <- new.env()
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
#'                     DEFAULT: `value`
#' @param fixed        Fixed match in [grepl]? DEFAULT: TRUE
#' @param ignore_space Remove spaces before comparison? DEFAULT: TRUE
#' @param ignore_quote Replace `'` with `"` before comparison? DEFAULT: TRUE
#'
rt_contains <- function(
object,
value,
msgval=value,
fixed=TRUE,
ignore_space=TRUE,
ignore_quote=TRUE
){
name <- deparse(substitute(object))
value2 <- value # to keep 'value' as is for warning message
if(is.character(value))
  {
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
  if(!succ) return(rt_warn("'",name,"' should contain '",msgval[i],"'."))
  }

# pass:
TRUE
}
