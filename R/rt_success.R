# Set or get task success status     -     Berry Boessenkool, Oct 2020 + 2021
# see https://github.com/openHPI/codeoceanR/tree/main/inst/extdata
# taskenvironment  is defined in rt_test_exercise

rt_success <- function(
	tnumber,                        # Task number (numeric)
	method="set",                   # "set" or "get"
	v=FALSE,                        # if "set", use this value
	msg="Please first solve task "  # if "get" and result is FALSE: Charstring with message to be prefixed to `tnumber`.
	){
if(!exists("taskenvironment")) stop("Cannot find taskenvironment.")
if(!is.numeric(tnumber))
	stop("tnumber (",toString(tnumber),") must be numeric, not ", toString(class(tnumber)))
if(method=="set")
  {
	taskenvironment$success[tnumber] <- v
  return(v)
  }
if(method=="get")
  {
  if(isTRUE(taskenvironment$success[tnumber])) return(TRUE)
  rt_warn(msg, tnumber)
  return(FALSE)
  } else
stop("method '",method,"' is not implemented.")
}

if(FALSE){ # Examples ------------------------------------------------------
try(rt_success(3))
taskenvironment <- new.env()
taskenvironment$success <- vector()
taskenvironment$task_id <- "x"
rt_success(3, v=TRUE)
taskenvironment$success
rt_success(3, "get")
rt_success(2, "get")
try(rt_success(2, "dummy"))
}
