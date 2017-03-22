#' Make a timeserie of dates
#'
#' The fuction \code{dateTimeSerie()} builds the timeserie of date
#' @param timeResolution "daily","hourly","minute","second"
#' @param fromPeriod date char "YYYYmmddH..."
#' @param toPeriod date char "YYYYmmdd..."
#' @param precision "daily","hourly","minute","second",...
#' @return The output is a list of a timeserie of POSIXct and the number of steps
#' @keywords timeManip
#' @export
#' @examples
#' res <- timeserie(timeResolution = "hourly",fromPeriod="2015010100",toPeriod="2015010322")
#' head(res)
#' res <- timeserie("daily","1980022802","1980030102","hourly")
#' head(res)

timeserie <- function(timeResolution,fromPeriod,toPeriod,precision=NULL){

  if (is.null(precision) && (timeResolution!="three-hourly")) {
    precision <- timeResolution
  } else if(is.null(precision) && (timeResolution=="three-hourly")) {
    precision <- "hourly"
  }
  from_POSIXlt <- standard(precision,fromPeriod)
  to_POSIXlt   <- standard(precision,toPeriod)

  seqPeriod <- switch(timeResolution,
    "yearly"       = seq(from = from_POSIXlt, to = to_POSIXlt, by = "year"),
    "monthly"      = seq(from = from_POSIXlt, to = to_POSIXlt, by = "month"),
    "daily"        = seq(from = from_POSIXlt, to = to_POSIXlt, by = (24*3600)),
    "three-hourly" = seq(from = from_POSIXlt, to = to_POSIXlt, by = (3*3600)),
    "hourly"       = seq(from = from_POSIXlt, to = to_POSIXlt, by = (3600)),
    "minute"       = seq(from = from_POSIXlt, to = to_POSIXlt, by = (60)),
    "second"       = seq(from = from_POSIXlt, to = to_POSIXlt, by = (1)),
    (message=paste0("Invalid time resolution:", timeResolution,"."))
  )
  nbStep <- length(seqPeriod)

  return(list(nbStep=nbStep,
              seqPeriod=as.POSIXlt(seqPeriod)))
}
