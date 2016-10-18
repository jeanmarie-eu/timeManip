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
#' res <- dateTimeSerie("hourly","20150101","20150103")
#' head(res)
#' res <- dateTimeSerie("hourly","19800228","19800301")
#' head(res)

dateTimeSerie <- function(timeResolution,fromPeriod,toPeriod){

   if (timeResolution=="daily") {
      # Daily
      rangeDate <- range(((strptime(fromPeriod,"%Y%m%d",tz="GMT"))),((strptime(toPeriod,"%Y%m%d",tz="GMT"))))
      dateTs <- zoo::zoo(,(as.Date(seq(from =rangeDate[1], to =rangeDate[2], by = "day"))))
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="monthly") {
      # Monthly
      rangeDate <- range(((strptime(paste(fromYear,fromMonth,"01",sep=""),"%Y%m%d",tz="GMT"))),((strptime(paste(toYear,toMonth,"01",sep=""),"%Y%m%d",tz="GMT"))))
      dateTs <- zoo::zoo(,(as.Date(seq(from =rangeDate[1], to =rangeDate[2], by = "month"))))
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="hourly") {
      # Hourly
      rangeDate <- range(((strptime(paste(fromPeriod,"000000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"230000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = "hour")))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="minute") {
      # Hourly
      rangeDate <- range(((strptime(paste(fromPeriod,"000000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"230000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = "min")))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   } else if (timeResolution=="second") {
      # Hourly
      rangeDate <- range(((strptime(paste(fromPeriod,"000000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))),((strptime(paste(toPeriod,"230000",sep=""),"%Y%m%d%H%M%S",tz="GMT"))))
      dateTs <- zoo::zoo(,(seq(from =rangeDate[1], to =rangeDate[2], by = "sec")))
      attributes(dateTs)$index <- base::as.POSIXct(attributes(dateTs)$index)
      seqPeriod <- attributes(dateTs)$index
      nbStep <- length(seqPeriod)
   }
   result <- list(nbStep=nbStep,
                  seqPeriod=seqPeriod)
   return(result)
}
