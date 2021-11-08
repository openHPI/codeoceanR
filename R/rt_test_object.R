#' @title check whether object has intended class, dimension, names and value
#' @return TRUE or FALSE, depending on whether full test is passed or failed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso Used in [rt_test_task]
#' @export
#' @param object,value object to be checked against value
#' @param name         Object name used in messages
#' @param class,intnum,dim,names,hasval,noise,stepwise,stepnames See [rt_test_task]
rt_test_object <- function(
object,
value,
name=deparse(substitute(object)),
class=NULL,
intnum=TRUE,
dim=TRUE,
names=TRUE,
hasval=TRUE,
noise=FALSE,
stepwise=NULL,
stepnames=FALSE
)
{

# class ----
if(is.null(class)) class <- class(value)
if(!rt_has_class(object, class, name=name, intnum=intnum)) return(FALSE)


if(!dim || is.function(value)) return(TRUE)
# all other tests are too error-prone if dim is unchecked.
# Should be TRUE to enable tests for class only by setting dim=FALSE

oneD <- length(dim(value))<2
isarr <- length(dim(value))>2 # assuming this will only happen for arrays...

# dim ----
if(dim)
  if(oneD) # vector, list, function, table
  {
  if(length(object)!=length(value))
    return(rt_warn("'",name,"' should have length ",length(value),", not ",length(object), "."))
  } else # dataframe, matrix, array
  {
  if(nrow(object)!=nrow(value))
    return(rt_warn("'",name,"' should have ",nrow(value)," rows, not ",nrow(object), "."))
  if(ncol(object)!=ncol(value))
    return(rt_warn("'",name,"' should have ",ncol(value)," columns, not ",ncol(object), "."))
  }
if(dim && isarr)
  {
	do <- dim(object)
	dv <- dim(value)
	if(length(do)!=length(dv)) return(rt_warn("'",name,"' should have ",length(dv)," dimensions, not ",length(do),"."))
	if(!rt_has_value(do, dv, paste0("dim(",name,")")) ) return(FALSE)
  }

# names ----
if(names)
  if(oneD) # 1D
  {
  if(!rt_has_value(names(object),names(value), name=paste0("names(",name,")"), stepwise=stepnames))
  	return(FALSE)
  } else # nD
  {
  if(!rt_has_value(colnames(object),colnames(value), name=paste0("colnames(",name,")"), stepwise=stepnames))
  	return(FALSE)
  if(!rt_has_value(rownames(object),rownames(value), name=paste0("rownames(",name,")"), stepwise=stepnames))
  	return(FALSE)
  if(isarr)
  if(!rt_has_value(dimnames(object),dimnames(value), name=paste0("dimnames(",name,")"), stepwise=stepwise))
  	return(FALSE)
  }

rcname <- function(r=NULL,c=NULL,l=NULL, fun=NULL)
  {
	# quotation marks if needed:
	qm <- function(i) if(is.numeric(i)) i else paste0('"',i,'"')
	out <- paste0(name,'[',qm(r),',',qm(c), if(!is.null(l))paste0(",",qm(l)),']')
	if(!is.null(fun)) out <- paste0(fun,"(",out,")")
	out
	}


if(!oneD) {loopcn <- colnames(value) ; if(is.null(loopcn)) loopcn <- 1:ncol(value)}
# class per column ----
if(!oneD && !is.array(value)) # not for matrix/array
  {
	for(cn in loopcn)
		if(!rt_has_class(object[,cn], class(value[,cn]),name=rcname(c=cn,fun="class"), intnum=intnum))
			return(FALSE)
  }

# hasval ----
if(hasval)
  if(oneD) # 1D
  {
  if(!rt_has_value(object,value, name=name, stepwise=stepwise)) return(FALSE)
  } else
  if(isarr) # 3D
  {
  looprn <- rownames(value) ; if(is.null(looprn)) looprn <- 1:nrow(value)
  loopln <- dimnames(value)[[3]] ; if(is.null(loopln)) loopln <- 1:dim(value)[3]
  # 4D arrays not checked - do so manually with rt_test_task(5,s,arr[,,,1],sol[,,,1]) && rt_test_task(5,s,arr[,,,2],sol[,,,2])
  for(rn in looprn) for(cn in loopcn) for(ln in loopln)
  if(!rt_has_value(object[rn,cn,ln], value[rn,cn,ln], name=rcname(rn,cn,ln), noise=noise, stepwise=FALSE)) return(FALSE)
  } else # 2D
  {
  for(cn in loopcn)
  	if(isTRUE(stepwise) || is.null(stepwise))
    {
  	for(rn in 1:nrow(value))
		  if(!rt_has_value(object[rn,cn], value[rn,cn], name=rcname(rn,cn), noise=noise)) return(FALSE)
    } else
    {
		if(!rt_has_value(object[,cn], value[,cn], name=rcname(c=cn), noise=noise, stepwise=FALSE)) return(FALSE)
    }
  }

return(TRUE) # if all tests passed :)
}
