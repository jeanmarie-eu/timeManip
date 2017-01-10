#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution choice between "daily","hourly","minute","second",...
#' @keywords timeManip
#' @export
#' @examples
#' timeManip(fromPeriod="2013060205",toPeriod="2013060205",timeResolution="hourly")
#'

timeManip  <- function(fromPeriod,toPeriod,timeResolution){
   myenv <- environment()

   fromPeriod <- fromPeriod
   toPeriod <- toPeriod
   timeResolution <- timeResolution
   Timeresinsec <- insec(timeResolution=timeResolution)
   tmp <- dateTimeSerie(timeResolution,fromPeriod,toPeriod)
   nbStep <- tmp$nbStep
   seqPeriod <- tmp$seqPeriod

   timeManipL = list(
       fromPeriod     = function(){return(get("fromPeriod",envir=myenv))},
       toPeriod       = function(){return(get("toPeriod",envir=myenv))},
       timeResolution = function(){return(get("timeResolution",envir=myenv))},
       Timeresinsec   = function(){return(get("Timeresinsec",envir=myenv))},
       nbStep         = function(){return(get("nbStep",envir=myenv))},
       seqPeriod      = function(){return(get("seqPeriod",envir=myenv))})

    assign('this',timeManipL,envir=myenv )
    class(timeManipL) <- append("timeManip","list")
    return(timeManipL)
}
