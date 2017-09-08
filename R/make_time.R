#' df2time
#'
#' df2time
#' @param precision choice between "daily","hourly","minute","second",...
#' @param df date.frame of the columns year, months,...
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' df <- data.frame(year=rep(2015,5),month=c(1,2,3,4,5),day=c(1,1,1,1,1),hour=c(1,2,3,4,5))
#' df2time(precision="hourly",df)


df2time <- function(precision,df){
  date <-switch(precision,
    "yearly"  = standard2(precision=precision, as.character(df[,1])),
    "monthly" = standard2(precision=precision, paste(as.character(df[,1]),as.character(df[,2]),sep="/")),
    "daily"   = standard2(precision=precision, paste(as.character(df[,1]),as.character(df[,2]),as.character(df[,3]),sep="/")),
    "hourly"  = standard2(precision=precision, paste(paste(as.character(df[,1]),as.character(df[,2]),as.character(df[,3]),sep="/"), as.character(df[,4]),sep=" ")),
    "minute"  = standard2(precision=precision, paste(paste(paste(as.character(df[,1]),as.character(df[,2]),as.character(df[,3]),sep="/"), as.character(df[,4]),sep=" "),as.character(df[,5]),sep=":")),
    "second"  = standard2(precision=precision, paste(paste(paste(paste(as.character(df[,1]),as.character(df[,2]),as.character(df[,3]),sep="/"), as.character(df[,4]),sep=" "),as.character(df[,5]),sep=":"),as.character(df[,6]),sep=":")),
    stop("invalid precision: ", precision))
  return(date)
}

#' standard2
#'
#' standard2
#' @param precision choice between "daily","hourly","minute","second",...
#' @param date_chr date in character as "2015/01/12 06:00"
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' res <- standard2("hourly","2015/01/01 00")
#' res <- standard2("monthly","1980/02")
#' res <- standard2("monthly","1980/2/1 06:12")

standard2 <- function(precision,date_chr){
  date <- switch(precision,
      "yearly"       = YYYY2_chr(date_chr=date_chr),
      "monthly"      = YYYYmm2_chr(date_chr=date_chr),
      "daily"        = YYYYmmdd2_chr(date_chr=date_chr),
      "hourly"       = YYYYmmddHH2_chr(date_chr=date_chr),
      "minute"       = YYYYmmddHHMM2_chr(date_chr=date_chr),
      "second"       = YYYYmmddHHMMSS2_chr(date_chr=date_chr),
      (stop(paste0("Invalid precision:", precision,"."))))
  return(date)
}


#' YYYY2_chr
#'
#' YYYY2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYY2_chr()
#' }
YYYY2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYY",date_chr))
}

#' YYYYmm2_chr
#'
#' YYYYmm2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmm2_chr()
#' }
YYYYmm2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYYmm",date_chr))
}

#' YYYYmmdd2_chr
#'
#' YYYYmmdd2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmdd2_chr()
#' }
YYYYmmdd2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYYmmdd",date_chr))
}

#' YYYYmmddHH2_chr
#'
#' YYYYmmddHH2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHH2_chr()
#' }
YYYYmmddHH2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYYmmddHH",date_chr))
}

#' YYYYmmddHHMM2_chr
#'
#' YYYYmmddHHMM2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHHMM2_chr()
#' }
YYYYmmddHHMM2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYYmmddHHMM",date_chr))
}

#' YYYYmmddHHMMSS2_chr
#'
#' YYYYmmddHHMMSS2_chr
#' @param date_chr date in character
#' @return The output is a POSIXlt value
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' res <- YYYYmmddHHMMSS2_chr()
#' }
YYYYmmddHHMMSS2_chr <- function(date_chr){
  return(char2POSIXlt2(char="YYYYmmddHHMMSS",date_chr))
}


char2POSIXlt2 <- function(char,date_chr){
  res <- switch(char,
    "YYYY" = as.POSIXlt(strptime(paste0(date_chr,"/01/01"),"%Y/%m/%d",tz="GMT")),
    "YYYYmm" = as.POSIXlt(strptime(paste0(date_chr,"/01"),"%Y/%m/%d",tz="GMT")),
    "YYYYmmdd" = as.POSIXlt(strptime(paste0(date_chr," 00:00:00"),"%Y/%m/%d %H:%M:%S",tz="GMT")),
    "YYYYmmddHH" = as.POSIXlt(strptime(paste0(date_chr,":00:00"),"%Y/%m/%d %H:%M:%S",tz="GMT")),
    "YYYYmmddHHMM" = as.POSIXlt(strptime(paste0(date_chr,":00"),"%Y/%m/%d %H:%M:%S",tz="GMT")),
    "YYYYmmddHHMMSS" = as.POSIXlt(strptime(date_chr,"%Y/%m/%d %H:%M:%S",tz="GMT")),
    (message=paste0("Invalid char:", char,"."))
   )
   return(res)
}
#################################################################
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
      (stop(paste0("Invalid precision:", precision,"."))))
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
