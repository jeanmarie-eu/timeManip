#' char2POSIXct
#'
#' char2POSIXct
#' @param timeResolution choice between "daily","hourly","minute","second", "monthly" ,"yearly"
#' @param date_chr "YYYY" or "YYYYmm" or "YYYYmmdd" or "YYYYmmddHH" or "YYYYmmddHHMM" or "YYYYmmddHHMMSS"
#' @return The output is a POSIXct value
#' @keywords timeManip
#' @export
#' @examples
#' res <- dateTimeSerie("hourly","2015010100")
#' res <- dateTimeSerie("monthly","198002")


char2POSIXct <- function(char,date_chr){

  if (char=="YYYY") {
     res <- strptime(paste(date_chr,"0101",sep=""),"%Y%m%d",tz="GMT")

  } else if (char=="YYYYMM") {
     res <- strptime(paste(date_chr,"01",sep=""),"%Y%m%d",tz="GMT")

  } else if (char=="YYYYmmdd") {
      res <- strptime(paste(date_chr,"000000",sep=""),"%Y%m%d%H%M%S",tz="GMT")

   } else if (char=="YYYYmmddHH") {
      res <- strptime(paste(date_chr,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT")

   } else if (char=="YYYYmmddHHMM") {
      res <- strptime(paste(date_chr,"00",sep=""),"%Y%m%d%H%M%S",tz="GMT")

   } else if (char=="YYYYmmddHHMMSS"){
      res <- strptime(date_chr,"%Y%m%d%H%M%S",tz="GMT")

   }

   return(as.POSIXct(res))
}
