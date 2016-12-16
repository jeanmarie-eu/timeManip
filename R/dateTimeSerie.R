#' Make a timeserie of dates
#'
#' The fuction \code{dateTimeSerie()} builds the timeserie of date
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param fromPeriod "YYYYmmddHHMMSS"
#' @param toPeriod "YYYYmmddHHMMSS"
#' @return The output is a list of a timeserie of POSIXct and the number of steps
#' @keywords timeManip
#' @export
#' @examples
#' res <- dateTimeSerie("hourly","2015010100","2015010322")
#' head(res)
#' res <- dateTimeSerie("hourly","1980022802","1980030107")
#' head(res)

dateTimeSerie <- function(timeResolution,fromPeriod,toPeriod){

   if (timeResolution=="daily") {
      rangeDate <- range(((strptime(paste(fromPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = (24*3600))))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="monthly") {
      rangeDate <- range(((strptime(paste(fromPeriod,"01",sep=""),"%Y%m%d",tz="GMT"))),((strptime(paste(toPeriod,"01",sep=""),"%Y%m%d",tz="GMT"))))
      dateTs <- zoo::zoo(,(as.Date(seq(from =rangeDate[1], to =rangeDate[2], by = "month"))))
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="hourly") {
      rangeDate <- range(((strptime(paste(fromPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = 3600)))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="three-hourly") {
      rangeDate <- range(((strptime(paste(fromPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"0000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = (3*3600))))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="minute") {
      rangeDate <- range(((strptime(paste(fromPeriod,"00",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"00",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = 60)))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="second") {
      rangeDate <- range(((strptime(fromPeriod,"%Y%m%d%H%M%S",tz="GMT"))),((strptime(toPeriod,"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = 1)))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   }
   result <- list(nbStep=nbStep,
                  seqPeriod=seqPeriod)
   return(result)
}
