
#' standard
#'
#' standard
#' @param precision choice between "daily","hourly","minute","second",...
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' res <- standard("hourly","2015010100")
#' res <- standard("monthly","198002")

standard <- function(precision,date_chr){
  date <- switch(precision,
      "yearly"       = YYYY_chr(date_chr=date_chr),
      "monthly"      = YYYYmm_chr(date_chr=date_chr),
      "daily"        = YYYYmmdd_chr(date_chr=date_chr),
      "hourly"       = YYYYmmddHH_chr(date_chr=date_chr),
      "minute"       = YYYYmmddHHMM_chr(date_chr=date_chr),
      "second"       = YYYYmmddHHMMSS_chr(date_chr=date_chr),
      (message=paste0("Invalid time resolution:", precision,".")))

  return(date)
}


#' YYYY_chr
#'
#' YYYY_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYY_chr()
#' }
YYYY_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYY",date_chr))
}

#' YYYYmm_chr
#'
#' YYYYmm_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmm_chr()
#' }
YYYYmm_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYYmm",date_chr))
}

#' YYYYmmdd_chr
#'
#' YYYYmmdd_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmdd_chr()
#' }
YYYYmmdd_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYYmmdd",date_chr))
}

#' YYYYmmddHH_chr
#'
#' YYYYmmddHH_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHH_chr()
#' }
YYYYmmddHH_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYYmmddHH",date_chr))
}

#' YYYYmmddHHMM_chr
#'
#' YYYYmmddHHMM_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHHMM_chr()
#' }
YYYYmmddHHMM_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYYmmddHHMM",date_chr))
}

#' YYYYmmddHHMMSS_chr
#'
#' YYYYmmddHHMMSS_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHHMMSS_chr()
#' }
YYYYmmddHHMMSS_chr <- function(date_chr){
  return(char2POSIXlt(char="YYYYmmddHHMMSS",date_chr))
}


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
   return(res)
}
