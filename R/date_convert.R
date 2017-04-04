
#' Get YYYY
#'
#' Get the year as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYY(res)
YYYY <- function(date){
  return(year(date,CHAR=TRUE))
}

#' Get YYYYmm
#'
#' Get the year and month as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmm(res)
YYYYmm <- function(date){
  return(paste0(year(date,CHAR=TRUE),month(date,CHAR=TRUE)))
}

#' Get YYYYmmdd
#'
#' Get the year, month and day as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmmdd(res)
YYYYmmdd <- function(date){
  return(paste0(year(date,CHAR=TRUE),month(date,CHAR=TRUE),day(date,CHAR=TRUE)))
}

#' Get YYYYmmddHH
#'
#' Get the year, month, day and hour as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmmddHH(res)
YYYYmmddHH <- function(date){
  return(paste0(year(date,CHAR=TRUE),month(date,CHAR=TRUE),day(date,CHAR=TRUE),hour(date,CHAR=TRUE)))
}

#' Get YYYYmmddHHMM
#'
#' Get the year, month, day, hour and minute as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHHMM_chr("201501010010")
#' YYYYmmddHHMM(res)
YYYYmmddHHMM <- function(date){
  return(paste0(year(date,CHAR=TRUE),month(date,CHAR=TRUE),day(date,CHAR=TRUE),hour(date,CHAR=TRUE),minute(date,CHAR=TRUE)))
}

#' Get YYYYmmddHHMMSS
#'
#' Get the year, month, day, hour, minute and second as character
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHHMMSS_chr("20150101001034")
#' YYYYmmddHHMMSS(res)
YYYYmmddHHMMSS <- function(date){
  return(paste0(year(date,CHAR=TRUE),month(date,CHAR=TRUE),day(date,CHAR=TRUE),hour(date,CHAR=TRUE),minute(date,CHAR=TRUE),second(date,CHAR=TRUE)))
}



#' Get YYYY_m
#'
#' Get the year as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYY_m(res)
YYYY_m <- function(date){
  return(cbind(year(date)))
}

#' Get YYYYmm_m
#'
#' Get the year and month as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmm_m(res)
YYYYmm_m <- function(date){
  return(cbind(year(date),month(date)))
}

#' Get YYYYmmdd_m
#'
#' Get the year, month and day as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmmdd_m(res)
YYYYmmdd_m <- function(date){
  return(cbind(year(date),month(date),day(date)))
}

#' Get YYYYmmddHH_m
#'
#' Get the year, month, day and hour as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHH_chr("2015010100")
#' YYYYmmddHH_m(res)
YYYYmmddHH_m <- function(date){
  return(cbind(year(date),month(date),day(date),hour(date)))
}

#' Get YYYYmmddHHMM_m
#'
#' Get the year, month, day, hour and minute as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHHMM_chr("201501010010")
#' YYYYmmddHHMM_m(res)
YYYYmmddHHMM_m <- function(date){
  return(cbind(year(date),month(date),day(date),hour(date),minute(date)))
}

#' Get YYYYmmddHHMMSS_m
#'
#' Get the year, month, day, hour, minute and second as a matrix
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- YYYYmmddHHMMSS_chr("20150101001034")
#' YYYYmmddHHMMSS_m(res)
YYYYmmddHHMMSS_m <- function(date){
  return(cbind(year(date),month(date),day(date),hour(date),minute(date),second(date)))
}


#' Convert the format of a POSIXct date
#'
#' Convert the format of a POSIXct date
#' SOON OBSOLETE
#' @param date format POSIXct
#' @param formatTo conversion into POSIXct, into a vector "YYMMDDHH" or a matrix "YY,MM,DD,HH"
#' @keywords timeManip
#' @export
#' @examples
#' res <- convertFormat(strptime("2016032701","%Y%m%d%H%",tz="GMT"),"YY,MM,DD,HH")
#' res <- convertFormat(strptime("2016032701","%Y%m%d%H%",tz="GMT"),"YYMMDDHH")

convertFormat <- function(date,formatTo){

  res <- switch(formatTo,
          "POSIXct"        = date,
          "YYMMDDHH"       = vectorFormat(date=date),
          "YY,MM,DD,HH"    = matrixFormat(date=date),
          (message=paste0("Invalid format expression:", formatTo,". Format conversion not yet coded"))
  )

   return(res)
}

vectorFormat <- function(date){
  res <- gsub("GMT","",gsub(":","",gsub("-","",date)))
  return(res)
}

matrixFormat <- function(date){
  datetmp <- base::as.POSIXlt(date)
  tmp <- list(year=(1900+datetmp$year),
              month=(1+datetmp$mon),
              day=datetmp$mday,
              hour=datetmp$hour)
  res <- cbind(tmp$year,tmp$month,tmp$day,tmp$hour)
  return(res)
}
