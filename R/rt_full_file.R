#' @title Get full file locally
#' @description Replace small sample version of a data file with complete one on local machine.\cr
#' Scoring is relatively slow for large files on CodeOcean. Have a small version there.
#' With this function, update to a large version locally for smooth graphics.
#' @return filename if replacement happened, FALSE with ff-prefixed [rt_warn] otherwise
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2022
#' @seealso \code{\link{help}}, \code{graphics::\link[graphics]{plot}}
#' @keywords file
# @importFrom package fun1 fun2
#' @export
#' @param \dots File name(s)
#' @param size [file.size] beyond which files are not replaced. DEFAULT: 1e4
#'
rt_full_file <- function(
...,
size=1e4
)
{
fn <- c(...)
name <- fn[1] # for first messages
ff <- function(..., not=TRUE)
  {
  m <- c(list("ff '",name,"' ", en=if(not)"not replaced: "  else "replaced",
  						                  de=if(not)"nicht ersetzt: " else "ersetzt"),
  			 list(...),".")
  m <- m[names(m) %in% c(rt_env()$lang, "")]
  paste(unlist(m),collapse="")
  }
if(Sys.getenv("CODEOCEAN")=="true") return(ff(en="on",de="auf"," browser-CodeOcean"))
wd <- paste0(getwd(),"/")
if(grepl("kurs/de_aufgaben/", wd)) return(ff("in German task development folder"))
if(grepl("kurs/en_exercises/", wd)) return(ff("in English task development folder"))
out <- vector()
for(name in fn)
{
if(!file.exists(name))
  {
  out <- c(out, ff(en="file does not exist", de="Datei existiert nicht"))
  next
  }
if(file.size(name) > size)
  {
  out <- c(out, ff(en="file is already big",de="Datei ist bereits gro\u00DF"))
  next
  }
pfile <- system.file(paste0("extdata/",name), package="codeoceanR")
if(pfile=="")
  {
	out <- c(out, ff(en="file is not in package. Consider running codeoceanR::rt_update_package()",
									 de="Datei ist nicht im Paket. Ggf. hilft codeoceanR::rt_update_package()"))
  next
  }
res <- file.copy(pfile, name, overwrite=TRUE)
if(res) out <- c(out, ff(not=FALSE)) else out <- c(out, ff("file.copy ",en="failed",de="fehlgeschlagen"))
}
return(out)
}
#   rt_full_file("R45d_Zugspitze.txt", "R46d_Meta.csv", "R46d_Gesundheit.csv")
