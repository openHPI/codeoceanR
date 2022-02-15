#' @title Warn about exercise submission time
#' @description Warn exercise participant about due time.
#' @return output of [rt_warn()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso [rt_warn()]
#' @export
#' @examples
#' rt_due_warn("2020-11-30 16:00")
#' rt_due_warn("2020-11-30 16:00", begin=1e9, end=1e9)
#'
#' rt_due_warn(Sys.time()+125, format="%F %T") # format for sub-minute accuracy
#' rt_due_warn(Sys.time()+30, format="%F %T")
#'
#' @param due   Charstring with due time in the format "2020-11-30 16:00".
#'              Remember 15:00 UTC is 16:00 CET.
#' @param tz    Timezone for [strptime]. DEFAULT: "CET"
#' @param format Format for [strptime]. DEFAULT: "%Y-%m-%d %H:%M"
#' @param begin Numeric: remaining time in minutes after which the message is displayed. DEFAULT: 90
#' @param end   Numeric: remaining time in minutes after which the message is no longer displayed. DEFAULT: -15
#' @param \dots Further arguments passed to [rt_warn()]
#'
rt_due_warn <- function(
due,
tz="CET",
format="%Y-%m-%d %H:%M",
begin=90,
end=-15,
...
)
{
due <- strptime(due, format=format, tz=tz)
remaining <- difftime(due, Sys.time(), units="mins")
remaining <- as.numeric(round(remaining,1))
rt_env(id="")
now <- remaining<1 && remaining>0
if(remaining<begin && remaining>end)
rt_warn(en="Submission is due in ",de="Die Abgabe ist f\u00E4llig in ", remaining,
				en=" mins.",de=" Minuten.",
				en=if(now) " --> Please submit now!", de=if(now) " --> Bitte jetzt einreichen!", ...)
}
