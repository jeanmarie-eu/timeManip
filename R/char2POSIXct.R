#' char2POSIXct
#'
#' char2POSIXct
#' @param char "YYYY" or "YYYYmm" or "YYYYmmdd" or "YYYYmmddHH" or "YYYYmmddHHMM" or "YYYYmmddHHMMSS"
#' @param date_chr date in character
#' @return The output is a POSIXct value
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXct("YYYYmmddHH","2015010100")
#' res <- char2POSIXct("YYYYmm","198002")


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
