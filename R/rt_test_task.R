#' @title Wrapper for test script for an individual task within an exercise
#' @details Tests are performed in an order for increasingly specific
#'          messages that do not give away the solution too early.\cr
#'          Arguments after \dots need to be specified by full name.
#'          `tnumber` must always be given, `object` and `value`
#'          must be given (can be NULL) so later custom tests are not matched to them.
#' @return TRUE or FALSE, indicating whether all tests passed. Also changes [rt_env] status.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param tnumber  Number of task. If numeric, used for pass/fail in [rt_env].
#' @param object   Object that needs to be created in student script.
#'                 Regular object name, not quoted. First checked for existence.
#'                 If `value` is given, checked with `correct` and `zero`,
#'                 then passed to [rt_test_object].
#'                 If a function, it can be tested with `inputs`. DEFAULT: NULL
#' @param value    Intended value. DEFAULT: NULL
#' @param \dots    Further tests, comma-separated, can be given at the end.
#'                 Need to yield TRUE/FALSE and take care of [rt_warn] individually.
#'                 The `rt_*` test functions are designed for just that :).
#' @param solved   Task number that must be solved before any other tests are run.
#'                 DEFAULT: NULL
#' @param correct  Custom value message for multiple choice tasks?
#'                 No further tests (except ...-tests) are run if correct=TRUE.
#'                 Do not set to TRUE for objects that cannot be sorted. DEFAULT: FALSE
#' @param igncase  If `correct=TRUE` and `object` is a string,
#'                 ignore upper/lowercase and spaces? DEFAULT: FALSE
#' @param zero     Check for pre-assigned objects (to 0) with special message?
#'                 Only needs to be FALSE if the intended value is 0. DEFAULT: TRUE
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`.
#'                 Run through [rt_has_class]. DEFAULT: NULL (class of `value`)
#' @param intnum   Should numeric and integer class be interchangable? DEFAULT: TRUE
#' @param dim      Check dimension (length or nrow+ncol)?
#'                 If FALSE, `names`, column classes and `hasval` are also not checked.
#'                 DEFAULT: TRUE
#' @param funname  Check for function name as character string? Can have brackets.
#'                 DEFAULT: FALSE
#' @param names    Check names (or rownames/colnames)?
#'                 If FALSE, column classes in data.frames are not checked!
#'                 DEFAULT: TRUE
#' @param hasval   Run [rt_has_value]? DEFAULT: TRUE
#' @param stepwise stepwise parameter in [rt_has_value]. TRUE for arrays. DEFAULT: NULL
#' @param stepnames stepwise parameter for names check. DEFAULT: NULL
#' @param section  Section number to be read with [rt_script_section] into `code`
#'                 which is then available in ... tests.
#'                 For manual evaluation, use [`eval`]`(`[`str2expression`]`(code))`,
#'                 not [`str2lang`], in case students use line breaks.
#'                 `section` DEFAULT: NULL
#' @param script   Exercise script content from [rt_run_script].
#'                 Passed to [rt_script_section]. DEFAULT: NULL
#' @param solcode  Solution code (charstring) to be checked with [rt_has_args]. DEFAULT: NULL
#' @param nameonly Literal checks? Passed to [rt_has_args]. DEFAULT: FALSE
#' @param alt      List of alternative arguments. Passed to [rt_has_args]. DEFAULT: NULL
#' @param opt      Vector of optional arguments. Passed to [rt_has_args]. DEFAULT: NULL
#' @param ignAssign Remove assignment part from code? Passed to [rt_has_args]. DEFAULT: FALSE
#' @param inputs   List or vector with (named) charstrings with code to be called
#'                 if `object` and `value` are functions.
#'                 Will be called with `eval(str2lang(paste0("object(",input_i,")")))`.
#'                 Object names within rt_test_exercise are not available within rt_test_task,
#'                 unless listed in `export`.
#'                 For single-argument functions, numerical input works fine, too.
#'                 DEFAULT: NULL
#' @param export   Character vector with object names to be assigned into evaluation environment.
#'                 DEFAULT: NULL
#'
rt_test_task <- function(
tnumber,
object=NULL,
value=NULL,
...,
solved=NULL,
correct=FALSE,
igncase=FALSE,
zero=TRUE,
class=NULL,
intnum=TRUE,
dim=TRUE,
funname=FALSE,
names=TRUE,
hasval=TRUE,
stepwise=NULL,
stepnames=FALSE,
section=NULL,
script=NULL,
solcode=NULL,
nameonly=FALSE,
alt=NULL,
opt=NULL,
ignAssign=FALSE,
inputs=NULL,
export=NULL
)
{
n <- deparse(substitute(object))
rt_env(id=tnumber)

# Exit this function through return() right after the first rt_warn message

# solved ----
if(!is.null(solved))
	if(!rt_test(rt_env()$success[solved], en="Please first solve task ", de="Bitte l\u00F6se zuerst Aufgabe ",solved,"."))
		return(rt_env(fail=tnumber))

# object created ----
if(n!="NULL" && !exists(n, envir=parent.frame()))
  {
  rt_warn(en="Create the object '", de="Erstelle das Objekt '",n,"'.")
	return(rt_env(fail=tnumber))
  }

# correct + zero + rt_test_object only if value is given:
if(!is.null(value))
{
# correct ----
if(correct && !is.function(value))
	{
	if(!is.atomic(object))
		{
	  rt_warn("'",n,en="' must be atomic (a vector), not of class '",
	  				de="' muss ein atomic vector sein, nicht class '",
	  				toString(class(object)), "'", de=" haben", ".")
	  return(rt_env(fail=tnumber))
	  }
	if(!rt_has_class(object, class(value), name=n, intnum=intnum))
			return(rt_env(fail=tnumber))
	if(igncase && is.character(tolower(object)))
	  {
		object <- gsub(" ", "", tolower(object), fixed=TRUE)
		value  <- gsub(" ", "", tolower(value ), fixed=TRUE)
	  }
	if(!isTRUE(all.equal(sort(object),sort(value))))
		{
		toString2 <- function(x)
      {
      if(is.character(x) && length(x)==1) x <- paste0('"',x,'"')
      if(is.null(names(value))) return(toString(x))
      if(is.null(names(x))) if(rt_env()$lang=="de")
      	return("angegeben: der Vektor muss Namen haben") else return("given: it should have names")
      paste0(names(x),"=",x, collapse=", ")
      }
	  rt_warn(en="The correct answer for '", de="Die richtige Antwort f\u00FCr '",n,
	  				en="' is not ",de="' ist nicht ", toString2(object), ".")
	  return(rt_env(fail=tnumber))
	  }
  }
if(correct){zero <- FALSE ; class <- "any" ; dim <- FALSE} # only run ...-tests after this

# zero ----
if(zero && identical(object, 0))
  {
  rt_warn(en="Replace the 0 for '",de="Ersetze die 0 f\u00FCr '",n,
  				en="' with the solution code.",de="' mit dem L\u00F6sungscode.")
	return(rt_env(fail=tnumber))
  }
if(zero && is.function(value) && identical(rt_gives("echo",object(),value=TRUE),0))
  {
  rt_warn("'",n,"()' ",en="should not return 0.",de="sollte nicht 0 zur\u00FCckgeben.")
  return(rt_env(fail=tnumber))
  }

# test_object ----
if(!rt_test_object(object, value, name=n, class=class, intnum=intnum, dim=dim, funname=funname, names=names,
	hasval=hasval, stepwise=stepwise, stepnames=stepnames)) return(rt_env(fail=tnumber))

} # end !null(value)


# code section ----
if(!is.null(section))
  {
	code <- rt_script_section(script, section, name=deparse(substitute(script)))
	if(isFALSE(code)) return(rt_env(fail=tnumber))
	if(!is.null(solcode) && !rt_has_args(code=code, target=solcode, snumber=section,
							nameonly=nameonly, class=class, intnum=intnum, dim=dim, names=names,
							hasval=hasval, stepwise=stepwise, stepnames=stepnames,
							alt=alt, opt=opt, ignAssign=ignAssign)) return(rt_env(fail=tnumber))
	assign("code", code, parent.frame()) # for further tests
  }

# function inputs ----
if(is.function(value) && !is.null(inputs))
{
for(e in export) assign(e, dynGet(e)) # get from parent frame (not parent env)
for(i in inputs)
  {
	vec <- function(x) if(length(x)==1) x else rt_vec(x)
  uc <- paste0("object(",vec(i),")") # user call
  cc <- paste0( "value(",vec(i),")") # correct call
  pc <- paste0(    n,"(",vec(i),")") # print call
	res    <- try(eval(str2lang(uc)), silent=TRUE)
	target <- try(eval(str2lang(cc)), silent=TRUE)
	if(inherits(res, "try-error"))
	  {
		res <- sub("^Error in .*?:", "", res)
		res <- gsub("\n", "", res)
    rt_warn("'",pc, en="' should not yield Error: ", de="' sollte nicht folgenden Fehler erzeugen: ", res)
    return(rt_env(fail=tnumber))
    }
  if(!rt_test_object(res, target, name=pc, class=class, intnum=intnum, dim=dim, names=names,
	  hasval=hasval, stepwise=stepwise, stepnames=stepnames)) return(rt_env(fail=tnumber))
	} # end for loop
}

# further tests ----
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_env(fail=tnumber))


# pass ----
# set rt_test_env$success[tnumber] to TRUE if all tests passed:
rt_env(pass=tnumber)
}
