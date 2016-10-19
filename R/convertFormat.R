#' Convert the format of a POSIXct date
#'
#' Convert the format of a POSIXct date
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
