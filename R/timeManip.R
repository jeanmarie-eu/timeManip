#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution "daily","hourly","minute","second",...
#' @param precision "daily","hourly","minute","second",...
#' @param Timeresinsec integer
#' @param nbStep integer
#' @param seqPeriod POSIXct timeserie
#' @keywords timeManip
#' @export
#' @examples
#' timeManip(fromPeriod="2013060205",toPeriod="2013060205",timeResolution="hourly",precision="hourly")
#'

timeManip  <- function(fromPeriod,toPeriod,timeResolution,precision=NULL,Timeresinsec=NULL,nbStep=NULL,seqPeriod=NULL){
   timeManip_object(fromPeriod,toPeriod,timeResolution,precision,Timeresinsec,nbStep,seqPeriod)
}

timeManip_object <- function(I_fromPeriod,I_toPeriod,I_timeResolution,I_precision,I_Timeresinsec,I_nbStep,I_seqPeriod){

  tserie <- NULL
  trinsec <- NULL

  if ((is.null(I_Timeresinsec)) && (is.null(I_nbStep)) && (is.null(I_seqPeriod))) {
    trinsec <- insec(timeResolution=I_timeResolution)
    tserie <- timeserie(I_timeResolution,I_fromPeriod,I_toPeriod,I_precision)
  } else {
    trinsec <- I_Timeresinsec
    tserie <- list(nbStep=I_nbStep,seqPeriod=I_seqPeriod)
  }

  object <- local({

     fromPeriod     = function(){return(I_fromPeriod)}
     toPeriod       = function(){return(I_toPeriod)}
     timeResolution = function(){return(I_timeResolution)}
     Timeresinsec   = function(){return(trinsec)}
     nbStep         = function(){return(tserie$nbStep)}
     seqPeriod      = function(){return(tserie$seqPeriod)}

     environment()
   })
   lockEnvironment(object, TRUE)
   structure(object, class=c("timeManip", class(object)))

}
