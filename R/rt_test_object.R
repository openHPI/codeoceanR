#' @title test objects in CodeOcean Setting
#' @description Diverse object tests calling [rt_warn()] with informative and helpful messages.
#' @return Logical: TRUE / FALSE depending on whether condition is met
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @importFrom stats rnorm
#' @name rt_test_object
#'
#' @param obj    Object to be tested
#' @param df     Object (data.frame) to be tested
#' @param class  Charstring passed to [inherits]
#' @param len    Value to which [length], [ncol] or [nrow] is compared
#' @param column Charstring to which [colnames] is compared
#' @param n      Charstring to which [names] is compared
#' @param target Object to be compared to via [identical]
#' @param value  value `obj` should have, can be char / numeric / other.
#' @param fixed  Fixed match in [grepl()]? DEFAULT: TRUE
#' @param digits Tolerance - both `obj` and `value` are [round]ed before comparison. DEFAULT: 6
#' @param noise  Add noise, so not the exact difference is reported? DEFAULT: TRUE
#' @param condition Generic custom condition to be tested
#' @param \dots     Message passed to [rt_warn] if condition evaluates to FALSE
#'
rt_exists <- function(obj) {
  obj <- deparse(substitute(obj)) # charstring of object name
  if(base::exists(obj)) return(TRUE)
  rt_warn("The object '",obj,"' does not exist.")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_class <- function(obj, class) {
  if(inherits(obj, class)) return(TRUE)
  rt_warn(deparse(substitute(obj)), " must be '", class, "', not of class '", toString(class(obj)), "'.")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_length <- function(obj, len) {if(length(obj)==len) return(TRUE)
  rt_warn(deparse(substitute(obj)), " must have length ", len,  ", not ", length(obj), ".")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_ncols <- function(df, len) {if(ncol(df)==len) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have ", len, " columns, not ", ncol(df), ".")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_nrows <- function(df, len) {if(nrow(df)==len) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have ", len, " rows, not ", nrow(df), ".")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_column <- function(df, column) {if(column %in% colnames(df)) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have the column ", column, ".")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_names <- function(obj, n) {if(all(n %in% names(obj))) return(TRUE)
  rt_warn(deparse(substitute(x)), " must have the name", if(length(n)>1) "s:", " ", toString(n), ".")
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_is_identical <- function(obj, target){
  if(identical(obj,target)) return(TRUE)
  rt_warn(deparse(substitute(obj)), " should be ", toString(target), " but is ", toString(obj))
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_contains <- function(obj, value, fixed=TRUE){
  objname <- deparse(substitute(obj))
  jup <- if(is.character(obj)) grepl(pattern=value, x=obj, fixed=fixed) else
                               value %in% obj
  if(jup) return(TRUE)
  rt_warn(objname, " does not contain ", toString(value))
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_has_value <- function(obj, value, digits=6, noise=TRUE){
  objname <- deparse(substitute(obj))

  if(is.character(obj)) if(all(obj==value)) return(TRUE) else
    {rt_warn(objname, " should be ", toString(value), " but is ", toString(obj));
    return(FALSE)}

  if(  all(is.na(value)) & all(is.na(obj))  ) return(TRUE)

  l <- length(obj)
  n <- sum(is.na(obj))
  if(n>0) {rt_warn(objname, if(l==1) " is NA." else paste0(" has NAs", " (", n, "/", l, ").")) ; return(FALSE)}

  obj <- round(obj, digits)
  val <- round(value, digits)
  dif <- obj - val
  if(noise) dif <- dif + round(rnorm(length(obj)),6)
  dif <- round(dif, digits)
  dif <- paste0("The deviance is",if(noise)" (approximately, with added noise)", ": ", toString(dif))
  if(all(obj==val)) return(TRUE)
  rt_warn(objname, " has the wrong value. ", dif)
  FALSE
}


#' @export
#' @rdname rt_test_object
rt_test <- function(condition, ...){
  if(condition) return(TRUE)
  rt_warn(...)
  FALSE
}

