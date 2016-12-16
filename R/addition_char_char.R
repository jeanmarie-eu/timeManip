#' Addition of a char date to a char value at a specific unit
#'
#' Addition of a char date to a char value at a specific unit
#'
#' @param char "YYYY" or "YYYYmm" or "YYYYmmdd" or "YYYYmmddHH" or "YYYYmmddHHMM" or "YYYYmmddHHMMSS"
#' @param date_chr date in character
#' @param v value
#' @param unit choice between daily","hourly","minute","second"
#' @keywords timeManip
#' @export
#' @examples
#' addition_char_char(char="YYYYmmddHH",date_chr="2013060205",v=3,unit="hourly")
#'

addition_char_char  <- function(char,date_chr,v,unit){
   d <- char2POSIXlt(char,date_chr)
   s <- insec(timeResolution=unit,v=v)
   res <- addition_plt_sec(date=d,sec=s)
   return(res)
}

addition_plt_sec <- function(date,sec){
   res <- date+sec
   return(res)
}
