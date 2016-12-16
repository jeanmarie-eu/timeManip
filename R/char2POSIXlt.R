#' char2POSIXlt
#'
#' char2POSIXlt
#' @param char "YYYY" or "YYYYmm" or "YYYYmmdd" or "YYYYmmddHH" or "YYYYmmddHHMM" or "YYYYmmddHHMMSS"
#' @param date_chr date in character
#' @return The output is a POSIXct value
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' res <- char2POSIXlt("YYYYmm","198002")


char2POSIXlt <- function(char,date_chr){

  res <- switch(char,
    "YYYY" = as.POSIXlt(strptime(paste0(date_chr,"0101"),"%Y%m%d",tz="GMT")),
    "YYYYmm" = as.POSIXlt(strptime(paste0(date_chr,"01"),"%Y%m%d",tz="GMT")),
    "YYYYmmdd" = as.POSIXlt(strptime(paste0(date_chr,"000000"),"%Y%m%d%H%M%S",tz="GMT")),
    "YYYYmmddHH" = as.POSIXlt(strptime(paste0(date_chr,"0000"),"%Y%m%d%H%M%S",tz="GMT")),
    "YYYYmmddHHMM" = as.POSIXlt(strptime(paste0(date_chr,"00"),"%Y%m%d%H%M%S",tz="GMT")),
    "YYYYmmddHHMMSS" = as.POSIXlt(strptime(date_chr,"%Y%m%d%H%M%S",tz="GMT")),
    (message=paste0("Invalid char:", char,"."))
   )
   return(as.POSIXlt(res))
}
