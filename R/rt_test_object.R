#' @title check whether object has intended class, dimension, names and value
#' @return TRUE or FALSE, depending on whether full test is passed or failed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso Used in [rt_test_task]
#' @export
#' @param object,value object to be checked against value
#' @param name         Object name used in messages
#' @param qmark        Include ' marks around `name`? DEFAULT: TRUE
#' @param class,intnum,dim,funname,names,hasval,stepwise,stepnames See [rt_test_task]
rt_test_object <- function(
object,
value,
name=deparse(substitute(object)),
qmark=TRUE,
class=NULL,
intnum=TRUE,
dim=TRUE,
funname=FALSE,
names=TRUE,
hasval=TRUE,
stepwise=NULL,
stepnames=FALSE
)
{
force(name)

# class ----
if(is.null(class)) class <- class(value)
if(!rt_has_class(object, class, name=name, intnum=intnum, qmark=qmark)) return(FALSE)


if(!dim || is.function(value)) return(TRUE)
# all other tests are too error-prone if dim is unchecked.
# Should be TRUE to enable tests for class only by setting dim=FALSE

oneD <- length(dim(value))<2
isarr <- length(dim(value))>2 # assuming this will only happen for arrays...

# printing name:
pn <- if(qmark) paste0("'", name, "'") else name
pn <- paste0(pn, " ")

# dim ----
if(dim)
  if(oneD) # vector, list, function, table
  {
  if(length(object)!=length(value))
    return(rt_warn(pn,en="should have length ",de="sollte ",length(value),en=", not ",
    							 de=" Elemente haben (length), nicht ",length(object), "."))
  } else # dataframe, matrix, array
  {
  if(nrow(object)!=nrow(value))
    return(rt_warn(pn,en="should have ",de="sollte ",nrow(value),en=" rows, not ",
    							 de=" Zeilen haben, nicht ",nrow(object), "."))
  if(ncol(object)!=ncol(value))
    return(rt_warn(pn,en="should have ",de="sollte ",ncol(value),en=" columns, not ",
    							 de=" Spalten haben, nicht ",ncol(object), "."))
  }
if(dim && isarr)
  {
	do <- dim(object)
	dv <- dim(value)
	if(length(do)!=length(dv)) return(rt_warn(pn,en="should have ",de="sollte ",
			  length(dv),en=" dimensions, not ",de=" Dimensionen haben, nicht ",length(do),"."))
	if(!rt_has_value(do, dv, paste0("dim(",name,")"), stepwise=FALSE, qmark=qmark) ) return(FALSE)
  }

# funname ----
if(funname)
{
 empty <- object == ""
 # browser()
 if(!empty && strsplit(object,"(", fixed=TRUE)[[1]][1] == value) return(TRUE)
 return(rt_warn(en="The correct answer for '", de="Die richtige Antwort f\u00FCr '",name,
                en="' is not '",de="' ist nicht '", toString(object), "'."))
}

# names ----
if(names)
  if(oneD) # 1D
  {
  truncM <- function(xx)
  	{
  	if(length(xx)==1) return(paste0("'",xx,"'"))
  	if(length(xx)>3) return(paste0(toString(xx[1:3]), ", [...]"))
  	return(toString(xx))
    }
  if(is.null(names(value)) && !is.null(names(object)) )
  	return(rt_warn(pn, en="should not have names, but has: ", de="sollte keine Namen haben, hat aber: ",
  								 truncM(names(object)), "."))
  if(!rt_has_value(names(object),names(value), name=paste0("names(",name,")"), stepwise=stepnames, qmark=qmark))
  	return(FALSE)
  } else # nD
  {
  if(!rt_has_value(colnames(object),colnames(value), name=paste0("colnames(",name,")"), stepwise=stepnames, qmark=qmark))
  	return(FALSE)
  if(!rt_has_value(rownames(object),rownames(value), name=paste0("rownames(",name,")"), stepwise=stepnames, qmark=qmark))
  	return(FALSE)
  if(isarr)
  if(!rt_has_value(dimnames(object),dimnames(value), name=paste0("dimnames(",name,")"), stepwise=stepwise, qmark=qmark))
  	return(FALSE)
  }

rcname <- function(r=NULL,c=NULL,l=NULL, fun=NULL)
  {
	# quotation marks if needed:
	qm <- function(i) if(is.null(i)) " " else if(is.numeric(i)) i else paste0('"',i,'"')
	out <- paste0(name,'[',qm(r),',',qm(c), if(!is.null(l))paste0(",",qm(l)),']')
	if(!is.null(fun)) out <- paste0(fun,"(",out,")")
	out
	}


if(!oneD) {loopcn <- colnames(value) ; if(is.null(loopcn)) loopcn <- 1:ncol(value)}
# class per column ----
if(names && !oneD && !is.array(value)) # not for matrix/array, not if names were unchecked
  {
	for(cn in loopcn)
		if(!rt_has_class(object[,cn], class(value[,cn]),name=rcname(c=cn), intnum=intnum, qmark=qmark))
			return(FALSE)
  }

# hasval ----
if(hasval)
  if(oneD) # 1D
  {
  if(!rt_has_value(object,value, name=name, stepwise=stepwise, qmark=qmark)) return(FALSE)
  } else
  if(isarr) # 3D
  {
  looprn <- rownames(value) ; if(is.null(looprn)) looprn <- 1:nrow(value)
  loopln <- dimnames(value)[[3]] ; if(is.null(loopln)) loopln <- 1:dim(value)[3]
  # 4D arrays not checked - do so manually with rt_test_task(5,s,arr[,,,1],sol[,,,1]) && rt_test_task(5,s,arr[,,,2],sol[,,,2])
  for(rn in looprn) for(cn in loopcn) for(ln in loopln)
  if(!rt_has_value(object[rn,cn,ln], value[rn,cn,ln], name=rcname(rn,cn,ln), stepwise=FALSE, qmark=qmark)) return(FALSE)
  } else # 2D
  {
  for(cn in loopcn)
  	if(isTRUE(stepwise) || is.null(stepwise))
    {
  	for(rn in 1:nrow(value))
		  if(!rt_has_value(object[rn,cn], value[rn,cn], name=rcname(rn,cn), qmark=qmark)) return(FALSE)
    } else
    {
		if(!rt_has_value(object[,cn], value[,cn], name=rcname(c=cn), stepwise=FALSE, qmark=qmark)) return(FALSE)
    }
  }

return(TRUE) # if all tests passed :)
}
