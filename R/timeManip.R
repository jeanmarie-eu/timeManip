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
#' res <- timeManip(fromPeriod="2013060205",toPeriod="2013060205",
#'                  timeResolution="hourly",precision="hourly")
#' res$seqPeriod()
timeManip  <- function(fromPeriod,toPeriod,timeResolution,precision=NULL){
   timeManip_object(I_fromPeriod=fromPeriod,I_toPeriod=toPeriod,I_timeResolution=timeResolution,I_precision=precision)
}

timeManip_object <- function(I_fromPeriod,I_toPeriod,I_timeResolution,I_precision){

  trinsec <- insec(timeResolution=I_timeResolution)
  FUN <- getFromNamespace(x="timeserie", ns="timeManip")

  object <- local({

     fromPeriod     = function(){
       return(I_fromPeriod)
     }

     toPeriod       = function(){
       return(I_toPeriod)
     }

     timeResolution = function(){
       return(I_timeResolution)
     }

     Timeresinsec   = function(){
       return(trinsec)
     }

     nbStep         = function(){
       return(FUN(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)$nbStep)
     }

     seqPeriod      = function(i=NULL){
       if (is.null(i)) {
         return(FUN(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)$seqPeriod)
       } else return(FUN(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)$seqPeriod[i])
     }

     environment()
   })
   lockEnvironment(object, TRUE)
   structure(object, class=c("timeManip", class(object)))

}
