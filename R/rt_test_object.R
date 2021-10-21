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
  }

rcname <- function(r=NULL,c=NULL, fun=NULL)
  {
	qm <- !is.numeric(c) # quotation marks needed?
	out <- paste0(name,'[',r,',',if(qm)'"',c,if(qm)'"',']')
	if(!is.null(fun)) out <- paste0(fun,"(",out,")")
	out
	}

# class per column ----
if(!oneD)
  {
	loopcn <- colnames(value)
	if(is.null(loopcn)) loopcn <- 1:ncol(value)
	for(cn in loopcn)
		if(!rt_has_class(object[,cn], class(value[,cn]),name=rcname(c=cn,fun="class"), intnum=intnum))
			return(FALSE)
  }

# hasval ----
if(hasval)
  if(oneD) # 1D
  {
  if(!rt_has_value(object,value, name=name, stepwise=stepwise)) return(FALSE)
  } else # nD
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
