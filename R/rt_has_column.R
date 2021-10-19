#' @title test object column
#' @description Test colnames of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the desired column(s).
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso `names` and `ncols` in [rt_test_task], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param df     data.frame-like object to be tested.
#' @param column Charstring (vector) to which [colnames] is compared
#' @param name   Object name for [rt_warn]. DEFAULT: object name
#'
rt_has_column <- function(df, column, name=deparse(substitute(df)))
{
force(name)
# presence of column(s):
for(cn in column) if(!cn %in% colnames(df))
		return(rt_warn("'",name,"' must have the column '", cn, "'."))
# order of columns:
if(!rt_has_value(colnames(df), column, name=paste0("colnames(",name,")"), stepwise=FALSE))
	return(FALSE)
TRUE
}
