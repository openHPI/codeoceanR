#' @title Examine student-written file
#' @description Test files written by students.
#' @return TRUE / FALSE, with [rt_warn] message.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2021
#' @seealso [rt_test_task]
#' @export
#' @examples
#' \dontrun{ # exclude file writing from CRAN checks
#' write.table(iris, "iri.txt")
#' iris2 <- iris ; iris2$Species <- as.character(iris2$Species)
#' rt_examine_file("iri.txt", sep="\t", value=iris2)
#' unlink("iri.txt")
#' }
#' @param fn        Name of file to be checked.
#' @param sep       Column separator that should have been used by students in
#'                  [write.table]. DEFAULT: NULL (not specifically checked,
#'                  any whitespace should be fine)
#' @param quote     T/F: should file contain quotes? DEFAULT: NULL (not checked)
#' @param dec       Decimal separator passed to [read.table]. DEFAULT: "." (not included in call)
#' @param saf       `stringsAsFactors` passed to [read.table]. DEFAULT: FALSE
#' @param header    `header` passed to [read.table]. DEFAULT: TRUE
#' @param rnames    T/F: should row.names have been used in [write.table]?
#'                  DEFAULT: NULL (not checked)
#' @param value     Object that should come up with `read.table(fn,header,sep,dec,saf)`.
#'                  Note: convert factors with [as.character] (unless saf=TRUE).
#'                  DEFAULT: NULL (not checked)
#' @param stepnames Passed to [rt_test_object]. DEFAULT: TRUE (unlike normally)
#' @param \dots     Further arguments passed to [rt_test_object].
#'
rt_examine_file <- function(
fn,
sep=NULL,
quote=NULL,
dec=".",
saf=FALSE,
header=TRUE,
rnames=NULL,
value=NULL,
stepnames=TRUE,
...
)
{
# existing?
if(!file.exists(fn)) return(rt_warn(en="The file '",de="Die Datei '",fn,
                                    en="' does not exist.",de="' existiert nicht."))
# not empty?
l1 <- readLines(fn, n=1)
if(length(l1)<1)     return(rt_warn(en="The file '",de="Die Datei '",fn,
                                    en="' should not be empty.",de="' sollte nicht leer sein."))

# right number of rows?
if(!is.null(value))
{
n_usr <- length(readLines(fn))
n_cor <- nrow(value) + as.numeric(header)
if(n_usr != n_cor)   return(rt_warn(en="The file '",de="Die Datei '",fn,
                                    en="' should have ",de="' sollte ", n_cor,
                                    en=" lines, not ", de=" Zeilen haben, nicht ", n_usr, "."))
}

# sep:
if(!is.null(sep) && sep=="\t") sep <- "\\t" # escape backslash for charstrings
if(!is.null(sep) && !grepl(sep,l1)) return(rt_warn(
              en="The column separator in '", de="Der Spaltentrenner in '", fn,
              en="' should be '",de="' sollte '",sep,en="'.",de="' sein."))
# quote:
if(!is.null(quote) && grepl('"',l1)!=quote) return(rt_warn(
              en="There should be ",de="Es sollten ",
              en=if(!quote)"no ",de=if(!quote)"keine ",
              en="quotation marks in '",de="Anf\u00FChrungsstriche enthalten sein in '", fn,"'."))
# can be read without errors?
cmd <- paste0('read.table("',fn, '"',
              if(header       )        ', header=TRUE',
              if(!is.null(sep)) paste0(', sep="',sep,'"'),
              if(dec!="."     ) paste0(', dec="',dec,'"'),
              if(saf)                  ', stringsAsFactors=TRUE', ')')
val <- rt_gives_echo(eval(str2lang(cmd))) # internal function, not exported!
if(val$captured!="") return(rt_warn(cmd, en=" raises the error ",de=" erzeugt den Fehler ", val$captured))
val <- val$value
# rownames:
if(!is.null(rnames))
 {
 cmd2 <- sub("\\)$", ", row.names=NULL)", cmd)
 val2 <- eval(str2lang(cmd2))
 hasrn <- !is.null(val2$row.names)
 if(hasrn!=rnames) return(rt_warn(
              en="The file '",de="Die Datei '",fn, en="' should ", de="' sollte ",
              en=if(!rnames)"not ",de=if(!rnames)"keine ",
              en="contain rownames.", de="Zeilennamen enthalten."))
 }
# colnames:
if(header && !is.null(value) && nrow(val)==nrow(value)-1) rt_warn(
              en="The file '",de="Die Datei '",fn,
              en="' should have column names.", de="' sollte Spaltennamen enthalten.")
# correct values?
if(!is.null(value) && !rt_test_object(val, value, name=cmd, stepnames=stepnames, ...)) return(FALSE)
# pass:
TRUE
}
