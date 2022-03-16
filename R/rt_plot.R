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
#' # Sys.setenv(CODEOCEAN="true")
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
#' @param maxn Maximum number of figures to plot. Note: rendering time increases drastically!
#' @export
rt_plot2 <- function(maxn=10)
{
if(Sys.getenv("CODEOCEAN")!="true") return(invisible())
graphics.off()
plotfiles <- dir(pattern="Rplot.*\\.png")
if(length(plotfiles)<1) return(invisible())
plotfiles2 <- plotfiles
if(length(plotfiles)>maxn)
  {
	if(rt_default_language=="de")
  warning(length(plotfiles), " Grafiken erstellt. Nur ", maxn," werden dargestellt. \u00C4ndere maxn f\u00FCr mehr.") else
  warning(length(plotfiles), " plots were created. Only plotting ", maxn,". Change maxn for more.")
  plotfiles2 <- plotfiles2[seq_len(maxn)]
  }
for(pf in plotfiles2)
  {
  if(file.size(pf) < 1) next
  cat(paste0('<img src="data:image/png;base64,',base64enc::base64encode(pf),'">\n\n'))
  }
unlink(plotfiles)
}
