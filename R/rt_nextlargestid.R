#' @title Find next largest 8 digit hexadecimal id
#' @description Internal function to find the next largest 8 digit hexadecimal id
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_add_opened_files()], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords internal
#' @examples
#' \dontrun{ # non-exported function cannot have autorun examples
#' # random hex name:
#' id <- paste0(sample(c(0:9,LETTERS[1:6]), 8, TRUE), collapse="")
#' id; rt_nextlargestid(id)
#' rt_nextlargestid("ABCDEFAB")
#' rt_nextlargestid("ABCD9999")
#' rt_nextlargestid("ABCD6789")
#' rt_nextlargestid("12345678")
#' rt_nextlargestid("A0000000")
#' }
#'
#' @param id Character string, see examples
#'
rt_nextlargestid <- function(id)
{
	if(length(id)==0) return("A0000000")
	idm <- max(id)
	if(!grepl("[0-8]", idm)) return("A0000000")
	addOne <- function(id)
	  {
		id <- strsplit(id,"")[[1]]
		wm <- max(which(id < 9))
		id[wm] <- as.numeric(id[wm]) + 1
		paste(id, collapse="")
	  }
	id <- idm
	while(id<=idm)  id <- addOne(id)
	return(id)
}
