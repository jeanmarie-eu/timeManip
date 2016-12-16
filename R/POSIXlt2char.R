#' POSIXlt2char
#'
#' POSIXlt2char
#' @param date POSIXlt date
#' @param char "YYYY" or "YYYYmm" or "YYYYmmdd" or "YYYYmmddHH" or "YYYYmmddHHMM" or "YYYYmmddHHMMSS"
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' POSIXlt2char(res,"YYYYmm")

POSIXlt2char <- function(date,char){
  date_chr <- gsub("[[:punct:][:blank:]]","",as.character(date))

  res <- switch(char,
    "YYYY" = substr(date_chr, 1, 4),
    "YYYYmm" = substr(date_chr, 1, 6),
    "YYYYmmdd" = substr(date_chr, 1, 8),
    "YYYYmmddHH" = substr(date_chr, 1, 10),
    "YYYYmmddHHMM" = substr(date_chr, 1, 12),
    "YYYYmmddHHMMSS" = substr(date_chr, 1, 14),
    (message=paste0("Invalid char:", char,"."))
   )
   return(res)
}
