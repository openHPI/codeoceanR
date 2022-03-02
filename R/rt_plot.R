#' @title handle graphics on CodeOcean
#' @description Display graphics in the CodeOcean Browser instance through base64 encoding.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2022
#' @seealso [png()], [base64enc::base64encode()]
#' @keywords file
#' @importFrom base64enc base64encode
#' @importFrom grDevices png graphics.off
#' @export
#' @rdname rt_plot
#' @aliases rt_plot1 rt_plot2
#' @examples
#' rt_plot1() # for graphics in CodeOcean Browser instance
#' plot(1:10)
#' 77 + 88
#' plot(rnorm(200))
#' rt_plot2() # preceding plots are displayed at the end of CO printout
#'
#' @param \dots Arguments passed to [png()]


rt_plot1 <- function(...)
{
if(Sys.getenv("CODEOCEAN")!="true") return(invisible())
png("Rplot%03d.png", ...)
}


#' @rdname rt_plot
#' @export
rt_plot2 <- function()
{
if(Sys.getenv("CODEOCEAN")!="true") return(invisible())
graphics.off()
plotfiles <- dir(pattern="Rplot.*\\.png")
if(length(plotfiles)<1) return(invisible())
for(pf in plotfiles)
  {
  if(file.size(pf) < 1) next
  cat(paste0('<img src="data:image/png;base64,',base64enc::base64encode(pf),'">'))
  }
unlink(plotfiles)
}
