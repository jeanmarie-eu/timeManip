#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution "daily","hourly","minute","second",...
#' @param precision "daily","hourly","minute","second",...
#' @keywords timeManip
#' @export
#' @examples
#' timeManip(fromPeriod="2013060205",toPeriod="2013060205",timeResolution="hourly",precision="hourly")
#'

timeManip  <- function(fromPeriod,toPeriod,timeResolution,precision=NULL){
   timeManip_object(I_fromPeriod=fromPeriod,I_toPeriod=toPeriod,I_timeResolution=timeResolution,I_precision=precision)
}

timeManip_object <- function(I_fromPeriod,I_toPeriod,I_timeResolution,I_precision){

  trinsec <- insec(timeResolution=I_timeResolution)

  object <- local({

     fromPeriod     = function(){return(I_fromPeriod)}
     toPeriod       = function(){return(I_toPeriod)}
     timeResolution = function(){return(I_timeResolution)}
     Timeresinsec   = function(){return(trinsec)}
     nbStep         = function(){return(timeserie(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)$nbStep)}
     seqPeriod      = function(){return(timeserie(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)$seqPeriod)}

     environment()
   })
   lockEnvironment(object, TRUE)
   structure(object, class=c("timeManip", class(object)))

}
