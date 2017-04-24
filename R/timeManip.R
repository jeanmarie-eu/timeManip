#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution "daily","hourly","minute","second",...
#' @param v multiplicative coefficient to the timeResolution
#' @param precision "daily","hourly","minute","second",...
#' @param tzone time zone
#' @keywords timeManip
#' @export
#' @examples
#' res <- timeManip(fromPeriod="2013060205",toPeriod="2013060205",
#'                  v=1,timeResolution="hourly",precision="hourly")
#' res$seqPeriod()
timeManip  <- function(fromPeriod,toPeriod,timeResolution,v=1,precision=NULL,tzone="GMT"){
   timeManip_object(I_fromPeriod=fromPeriod,I_toPeriod=toPeriod,I_timeResolution=timeResolution,I_v=v,I_precision=precision,I_tzone=tzone)
}

timeManip_object <- function(I_fromPeriod,I_toPeriod,I_timeResolution,I_v,I_precision,I_tzone){

  trinsec <- insec(timeResolution=I_timeResolution)
  if (is.null(I_precision)) {
    p_precision <- I_timeResolution
  } else p_precision <- I_precision
  p_fromPeriod <- timeManip::YYYYmmddHHMMSS(standard(precision=p_precision, I_fromPeriod))
  p_toPeriod <- timeManip::YYYYmmddHHMMSS(standard(precision=p_precision, I_toPeriod))

  FUN <- getFromNamespace(x="timeserie", ns="timeManip")

  object <- local({

     fromPeriod <- function(){
       return(p_fromPeriod)
     }

     toPeriod <- function(){
       return(p_toPeriod)
     }

     timeResolution <- function(){
       return(I_timeResolution)
     }

     precision <- function(){
       return(p_precision)
     }

     Timeresinsec <- function(){
       return(trinsec)
     }

     tzone <- function(){
       return(I_tzone)
     }

     nbStep <- function(){
       return(FUN(timeResolution=I_timeResolution,v=I_v,fromPeriod=p_fromPeriod,toPeriod=p_toPeriod,precision=p_precision)$nbStep)
     }

     seqPeriod <- function(i=NULL){
       if (is.null(i)) {
         return(FUN(timeResolution=I_timeResolution,v=I_v,fromPeriod=p_fromPeriod,toPeriod=p_toPeriod,precision=p_precision)$seqPeriod)
       } else return(FUN(timeResolution=I_timeResolution,v=I_v,fromPeriod=p_fromPeriod,toPeriod=p_toPeriod,precision=p_precision)$seqPeriod[i])
     }

     summary <- function(){
       res <- list(fromPeriod = p_fromPeriod,
                   toPeriod   = p_toPeriod,
                   timeResolution = I_timeResolution,
                   v = I_v,
                   precision =    p_precision,
                   timeresinsec = trinsec,
                   tzone =  I_tzone)
      return(res)
     }


     environment()
   })
   lockEnvironment(object, TRUE)
   structure(object, class=c("timeManip", class(object)))

}
