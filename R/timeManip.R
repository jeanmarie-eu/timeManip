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

timeManip  <- function(fromPeriod,toPeriod,timeResolution,precision,Timeresinsec=NULL,nbStep=NULL,seqPeriod=NULL){

   timeManip_object(fromPeriod,toPeriod,timeResolution,precision,Timeresinsec,nbStep,seqPeriod)

}


timeManip_object <- function(fromPeriod,toPeriod,timeResolution,precision,Timeresinsec,nbStep,seqPeriod){

  if ((is.null(Timeresinsec)) && (is.null(nbStep)) && (is.null(seqPeriod))) {
    Timeresinsec <<- insec(timeResolution=timeResolution)
    tmp <- timeserie(timeResolution,fromPeriod,toPeriod,precision)
    nbStep <<- tmp$nbStep
    seqPeriod <<- tmp$seqPeriod
  }

  object <- local({

     fromPeriod     = function(){return(fromPeriod)}
     toPeriod       = function(){return(toPeriod)}
     timeResolution = function(){return(timeResolution)}
     Timeresinsec   = function(){return(Timeresinsec)}
     nbStep         = function(){return(nbStep)}
     seqPeriod      = function(){return(seqPeriod)}

     environment()
   })
   lockEnvironment(object, TRUE)
   structure(object, class=c("timeManip", class(object)))

}
