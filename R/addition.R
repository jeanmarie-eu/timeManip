#' Addition of a value to a date
#'
#' The function \code{addition} enables the addition of a value to a date.
#' The date has no format, nor any time zone.
#' @param date string of character yyyymmdd
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param v positive (resp.negative) real value to be added (resp. substracted).
#' @keywords timeManip
#' @export
#' @examples
#' res <- addition("20150728","hourly",14)

addition  <- function(date,timeResolution,v){

   v <- insec(timeResolution=timeResolution,v=v)
   res <- base::strptime(date,"%Y%m%d",tz="GMT")+v

   return(res)
}
