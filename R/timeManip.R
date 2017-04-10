#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution "daily","hourly","minute","second",...
#' @param precision "daily","hourly","minute","second",...
#' @param tzone time zone
#' @keywords timeManip
#' @export
#' @examples
#' res <- timeManip(fromPeriod="2013060205",toPeriod="2013060205",
#'                  timeResolution="hourly",precision="hourly")
#' res$seqPeriod()
timeManip  <- function(fromPeriod,toPeriod,timeResolution,precision=NULL,tzone="GMT"){
   timeManip_object(I_fromPeriod=fromPeriod,I_toPeriod=toPeriod,I_timeResolution=timeResolution,I_precision=precision,I_tzone=tzone)
}

timeManip_object <- function(I_fromPeriod,I_toPeriod,I_timeResolution,I_precision,I_tzone){

  trinsec <- insec(timeResolution=I_timeResolution)
  if (is.null(I_precision)) {
    I_precision <- I_timeResolution
  }
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

     precision = function(){
       return(I_precision)
     }

     Timeresinsec   = function(){
       return(trinsec)
     }

     tzone         = function(){
       return(I_tzone)
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
