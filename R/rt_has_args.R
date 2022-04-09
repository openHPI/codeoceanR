#' @title Check code chunk with function call for correct arguments, e.g. in plotting tasks.
#' @description Check student code for certain argument values. Matching is done internally.\cr\cr
#' **Note**: in custom tests, use [str2expression] instead of [str2lang] to handle code with line breaks.\cr
#' Here, they are collapsed with `\n` as code sections are expected to have one single command only.
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2021
#' @seealso [str2lang], [match.call]
#' @importFrom grDevices xy.coords
#' @export
#' @examples
#' rt_has_args("plot(1:5, lwd=2)", "plot(1:5, lwd=3)", 7)
#' # Tx: code section t7: argument 'lwd' should be '3', not '2'.
#'
#' # 'data' arguments are attached internally, hence this works:
#' rt_has_args("plot(rate~conc,data=Puromycin,col=state)",
#'             "plot(rate~conc,data=Puromycin,col=state)", 7)
#'
#' rt_has_args("plot(1:5, lwd=2)", "plot(1:5, lwd=3)", 7, alt=list(lwd=2:4)) # TRUE
#'
#' rt_has_args("plot(c(1,2,3,4,5))", "plot(1:5)", 7               ) # TRUE
#' rt_has_args("plot(c(1,2,3,4,5))", "plot(1:5)", 7, nameonly=TRUE) # FALSE:
#' # Tx: code section t7: argument 'x' should be '1:5', not 'c(1, 2, 3, 4, 5)'.
#'
#' cv <- c("orange", "mediumpurple2")
#' rt_has_args('plot(1, xlab=paste0("Hi"))', 'plot(1, xlab="Hi")', 7) # TRUE
#' rt_has_args('plot(1, xlab=paste0("Ho"))', 'plot(1, xlab="Hi")', 7) # FALSE
#' rt_has_args('length(1:2)','length(cv)', 7)                # F: class char not int
#' rt_has_args('length(1:2)','length(cv)', 7, nameonly=TRUE) # F: 'cv' not '1:2'
#'
#' # See more in unit tests at "codeoceanR/tests/testthat/argtests.R"
#'
#' @param code     Charstring with user (student) code.
#' @param target   Charstring with desired code.
#' @param snumber  Section number for messaging.
#' @param nameonly Check for e.g. 1:5 exactly, disallowing c(1,2,3,4,5).
#'                 DEFAULT: FALSE
#' @param class,intnum,dim,names,hasval,stepwise,stepnames Passed to [rt_test_object], see [rt_test_task]
#' @param alt      list of alternately accepted inputs.
#'                 Use list(argname="anyval") to skip value test. DEFAULT: NULL
#' @param opt      Charstring vector of arguments that are optional.
#'                 Mostly for the formula interface, I guess. DEFAULT: NULL
#' @param ignAssign Remove assignment part from code? DEFAULT: FALSE
rt_has_args <- function(
code,
target,
snumber,
nameonly=FALSE,
class=NULL,
intnum=TRUE,
dim=TRUE,
names=TRUE,
hasval=TRUE,
stepwise=NULL,
stepnames=FALSE,
alt=NULL,
opt=NULL,
ignAssign=FALSE
)
{
cs <- paste0(if(rt_env()$lang=="de") "Code Abschnitt t" else "code section t", snumber)

# Language object from character string:
if(length(code)>1) code <- paste(code, collapse="\n")
if(ignAssign) code <- sub("^.*?<-","", code) # https://stackoverflow.com/a/9704260
code2 <- try(str2lang(code), silent=TRUE)
if(inherits(code2,"try-error"))
	if(grepl("parsing result not of length one", code2))
	return(rt_warn(cs, en=" may contain one single command only, not ",
                 de=" darf nur einen einzigen Befehl enthalten, nicht ",
                 strsplit(attr(code2,"condition")$message, "but ")[[1]][2], ".")) else
	return(rt_warn("str2lang ",en="for ",de="f\u00FCr ",cs,en=" produced error: ",
								 de=" erzeugte einen Fehler: ", attr(code2,"condition")$message))
target <- gsub("\\\\", "\\\\\\\\", target)
target <- str2lang(target)

# User function
u_fun <- try(code2[[1]], silent=TRUE)
if(inherits(u_fun,"try-error"))
	return(rt_warn(cs,en=" could not be executed. ",de=" kann nicht ausgef\u00FChrt werden. ",
  "str2lang(code)[[1]] ",en="produced: ",de="erzeugt: ", attr(u_fun,"condition")$message))

# Intended solution:
i_fun <- target[[1]]
if(!identical(i_fun, u_fun)) return(rt_warn(
	cs,en=" should contain the function '", de=" sollte die Funktion '",
	deparse(i_fun),en="', not '", de="' enthalten, nicht '",deparse(u_fun),"'."))

# user and intended arguments
# args to avoid invalid 'definition' argument error: https://stackoverflow.com/a/63124703
# all args, except function name:
u_arg <- try(as.list(match.call(args(eval(u_fun)), code2))[-1], silent=TRUE)
if(inherits(u_arg,"try-error"))
	return(rt_warn(cs,en=" could not be executed. ",de=" kann nicht ausgef\u00FChrt werden. ",
  "match.call(args(eval(u_fun)),code) ",en="produced: ",de="erzeugt: ", attr(u_arg,"condition")$message))
i_arg <- as.list(match.call(args(eval(i_fun)), target))[-1]

if(is.null(names(i_arg))&&length(i_arg)>0) return(rt_warn(
	cs, en=": argument names cannot be matched in trainer code. Please report this to Berry.",
	de=": Die Argumentnamen im Trainer-Code k\u00F6nnen nicht zugeordnet werden. Bitte melde dies Berry."))
if(is.null(names(u_arg))&&length(u_arg)>0) return(rt_warn(
	en="Arguments for '",de="Argumente f\u00FCr ",deparse(u_fun),
	en="' must be named explicitely in ", de="' m\u00FCssen namentlich angegeben werden in ", cs,"."))

# Duplicated arguments:
dup <- duplicated(names(u_arg)[names(u_arg)!=""])
if(any(dup)) return(rt_warn(
	cs,en=" should not contain the argument '", de=" sollte das Argument '",
	toString(unique(names(u_arg)[dup])),en="' more than once.", de="' nicht mehrfach enthalten."))

# copy objects from student-script here so eval(x) will find them:
# obs <- ls(1)
obs <- ls(parent.frame(2))
obs <- obs[!obs %in% c("expr")]
for(n in obs) assign(n, get(n,parent.frame(2)))
attachdata <- try(attach(eval(u_arg$data), warn.conflicts=FALSE), silent=TRUE)
if(inherits(attachdata, "try-error")) return(rt_warn(en="Not a valid value for argument 'data': ",
    de="Kein g\u00FCltiger Wert f\u00FCr das Argument 'data': ", u_arg$data))

# Evaluate/deparse arguments:
# eval environment in formula call?
argfun <- function(x)
  {
  if(is.character(x)) return(dQuote(x, '"'))
  if(nameonly       ) return(deparse(x)    )
  y <- eval(x)
  if(is.character(y)) y <- dQuote(y, '"')
  return(y)
  }
u_arg <- try(lapply(u_arg, argfun), silent=TRUE)
i_arg <- try(lapply(i_arg, argfun), silent=TRUE)
if(inherits(u_arg,"try-error")) return(rt_warn(
	cs,en=" could not be evaluated: ", de=" konnte nicht ausgef\u00FChrt werden: ",
	sub("\n$","",u_arg),
	en=if(grepl("nt is missing",u_arg)) ". This indicates code like plot(x,y,) with no last arg.",
	de=if(grepl("nt is missing",u_arg)) ". Dies weist hin auf Code wie plot(x,y,) ohne letztes Argument."))
if(inherits(i_arg,"try-error")) return(rt_warn(en="solution code for ", de="Musterl\u00F6sung f\u00FCr ",
	cs,en=" could not be evaluated: ", de=" konnte nicht ausgef\u00FChrt werden: ", sub("\n$","",i_arg)))

# if not named, formula is matched to 'x' in plot & boxplot, to 'height' in barplot
formula2xy <- function(xx)
  {
  if(inherits(xx[["x"     ]], "formula")) names(xx)[names(xx)=="x"     ] <- "formula"
  if(inherits(xx[["height"]], "formula")) names(xx)[names(xx)=="height"] <- "formula"
  if("formula" %in% names(xx) && ! "x" %in% names(xx) && ! "y" %in% names(xx))
    {
    coord <- suppressWarnings(try(xy.coords(xx$formula), silent=TRUE))
    if(inherits(coord, "try-error"))
    {
    rt_warn("Evaluation of formula '", deparse(xx$formula), "': ", coord)
    coord <- list(x=NULL, y=NULL)
    }
    xx$x <- coord$x
    xx$y <- coord$y
    xx$formula <- NULL
    xx$data <- NULL
    }
  xx
  }
u_arg <- formula2xy(u_arg)
i_arg <- formula2xy(i_arg)

# Presence and value of arguments:
for(n in names(i_arg))
  {
  if(!n %in% opt && !n %in% names(u_arg)) return(rt_warn(
  	cs, en=" should contain the argument '", de=" sollte das Argument '",
  	n, en="'.",de="' enthalten."))
	inalt <- try(u_arg[[n]] %in% alt[[n]], silent=TRUE)
	if(length(inalt)<1) inalt <- FALSE # catch NULL arguments
	inalt <- suppressWarnings(all(inalt))
	if(n %in% opt && !n %in% names(u_arg)) inalt <- TRUE
	if(isTRUE(alt[[n]]=="anyval") || isTRUE(inalt)) next
	esc <- function(x)
		{
		if(!is.character(x)) return(x)
		#x <- gsub("\\\\", "\\\\\\\\", x) # does not work
		if(identical(x,'"\t"')) x <- '"\\t"' # hard-coded
		if(identical(x,'"\n"')) x <- '"\\n"'
		if(identical(x,'"\\"')) x <- '"\\\\"'
		return(x)
	  }
	if(!rt_test_object(esc(u_arg[[n]]), esc(i_arg[[n]]), name=paste0(cs,": argument '",n,"'"),
										 qmark=FALSE, class=class, intnum=intnum, dim=dim, names=names,
										 hasval=hasval, stepwise=stepwise, stepnames=stepnames)) return(FALSE)
  }

# If all arguments are correct:
return(TRUE)
}
