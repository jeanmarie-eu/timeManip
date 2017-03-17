#' Addition of a POSIXlt date to a value not in second
#'
#' Addition of a POSIXlt date to a value not in second
#'
#' @param date POSIXlt date
#' @param timeResolution "daily","hourly","minute","second"
#' @param v value
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' addition_nonsec(date=res,"hourly",v=10)
#'
addition_nonsec <- function(date,timeResolution,v){
  sec <- insec(timeResolution=timeResolution,v=v)
  res <- addition(date=date,sec=sec)
  return(res)
}

#' Addition of a POSIXlt date to a value in second
#'
#' Addition of a POSIXlt date to a value in second
#'
#' @param date POSIXlt date
#' @param sec value in second
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' addition(date=res,sec=487654)
#'
addition <- function(date,sec){
   res <- date+sec
   return(as.POSIXlt(res))
}


#' Difference between two dates
#'
#' Difference in seconds between two dates
#' @param date1 date POSIXlt
#' @param date2 date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' difference()
#' }
difference  <- function(date1,date2){
   res <-base::difftime(date1, date2, units = "sec")
   return(res)
}
