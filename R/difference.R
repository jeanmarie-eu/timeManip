#' Difference between two dates
#'
#' Difference in seconds between two dates
#' @param date1 date POSIXct
#' @param date2 date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' difference()
#' }
difference  <- function(date1,date2){
   res <- base::as.POSIXct(c(base::difftime(date2, date1, units = "sec")), origin = base::strptime("00000101","%Y%m%d",tz="GMT"))
   return(res)

}
