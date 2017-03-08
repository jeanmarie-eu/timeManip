#' standard
#'
#' standard
#' @param timeResolution choice between "daily","hourly","minute","second",...
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' res <- standard("hourly","2015010100")
#' res <- standard("monthly","198002")

standard <- function(timeResolution,date_chr){
  date <- switch(timeResolution,
      "yearly"       = timeManip::char2POSIXlt(char="YYYY",date_chr=date_chr),
      "monthly"      = timeManip::char2POSIXlt(char="YYYYmm",date_chr=date_chr),
      "weekly"       = timeManip::char2POSIXlt(char="YYYYmmdd",date_chr=date_chr),
      "daily"        = timeManip::char2POSIXlt(char="YYYYmmdd",date_chr=date_chr),
      "three-hourly" = timeManip::char2POSIXlt(char="YYYYmmddHH",date_chr=date_chr),
      "hourly"       = timeManip::char2POSIXlt(char="YYYYmmddHH",date_chr=date_chr),
      "minute"       = timeManip::char2POSIXlt(char="YYYYmmddHHMM",date_chr=date_chr),
      "second"       = timeManip::char2POSIXlt(char="YYYYmmddHHMMSS",date_chr=date_chr),
      (message=paste0("Invalid time resolution:", timeResolution,".")))

  return(date)
}
