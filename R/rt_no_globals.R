#' @title Check that a function does not use objects from the global environment
#' @description Detect whether a student-defined function references objects
#' defined at script level (globals) instead of using its own arguments/locals.
#' Default argument values such as `function(x, y=default_val)` are stripped
#' before scanning, so they are never flagged (only the function body is).
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2026, created by Claude sonnet 5
#' @seealso Used in [rt_test_object], [codetools::findGlobals]
#' @export
#' @importFrom codetools findGlobals
#' @examples
#' default_val <- 5
#' f1 <- function(x, y=default_val) x + y   # fine: default value, not flagged
#' rt_no_globals(f1, "f1")
#'
#' f2 <- function(x) x + default_val        # uses a global inside the body
#' rt_no_globals(f2, "f2")
#'
#' rt_no_globals(f2, "f2", globalok="default_val") # explicitly allowed
#'
#' @param fun      Function to check, as defined/sourced in the student script.
#' @param name     Object name used in messages.
#' @param globalok Character vector of object names that are allowed to be
#'                 used as globals within `fun`. DEFAULT: NULL
#' @param qmark    Include ' marks around `name`? DEFAULT: TRUE
rt_no_globals <- function(fun, name, globalok=NULL, qmark=TRUE)
{
pn <- if(qmark) paste0("'", name, "'") else name
# Strip default argument values before scanning, since findGlobals()
# would otherwise flag e.g. 'default_val' in function(x, y=default_val).
# A plain body(fun) cannot be scanned directly: findGlobals()/collectUsage()
# needs an actual closure (with formals) to know which names are parameters.
ff <- formals(fun)
if(length(ff)>0) for(nm in names(ff)) ff[[nm]] <- quote(expr=)
formals(fun) <- ff
g <- suppressWarnings(codetools::findGlobals(fun, merge=TRUE))
scriptobjs <- ls(environment(fun), all.names=TRUE)
bad <- g[g %in% scriptobjs & !g %in% globalok]
if(length(bad)>0) return(rt_warn(
  en="Do not use the global object '", de="Verwende nicht das globale Objekt '",
  bad[1], en="' in the function ", de="' in der Funktion ", pn, "."))
TRUE
}
