#' timeManip
#'
#' timeManip
#'
#' @param fromPeriod dateChr "YYYYmmddHH..."
#' @param toPeriod dateChr "YYYYmmddHH..."
#' @param timeResolution choice between "daily","hourly","minute","second",...
#' @param Timeresinsec integer
#' @param nbStep integer
#' @param seqPeriod POSIXct timeserie
#' @keywords timeManip
#' @export
#' @examples
#' timeManip(fromPeriod="2013060205",toPeriod="2013060205",timeResolution="hourly")
#'

timeManip  <- function(fromPeriod,toPeriod,timeResolution,Timeresinsec=NULL,nbStep=NULL,seqPeriod=NULL){
   myenv <- environment()

   if ((is.null(Timeresinsec)) && (is.null(nbStep)) && (is.null(seqPeriod))) {
     myfromPeriod <- fromPeriod
     mytoPeriod <- toPeriod
     mytimeResolution <- timeResolution
     myTimeresinsec <- insec(timeResolution=timeResolution)
     tmp <- dateTimeSerie(mytimeResolution,myfromPeriod,mytoPeriod)
     mynbStep <- tmp$nbStep
     myseqPeriod <- tmp$seqPeriod
   } else {
     myfromPeriod <- fromPeriod
     mytoPeriod <- toPeriod
     mytimeResolution <- timeResolution
     myTimeresinsec <- Timeresinsec
     mynbStep <- nbStep
     myseqPeriod <- seqPeriod
   }

   timeManipL = list(
       fromPeriod     = function(){return(get("myfromPeriod",envir=myenv))},
       toPeriod       = function(){return(get("mytoPeriod",envir=myenv))},
       timeResolution = function(){return(get("mytimeResolution",envir=myenv))},
       Timeresinsec   = function(){return(get("myTimeresinsec",envir=myenv))},
       nbStep         = function(){return(get("mynbStep",envir=myenv))},
       seqPeriod      = function(){return(get("myseqPeriod",envir=myenv))})

    assign('this',timeManipL,envir=myenv )
    class(timeManipL) <- append("timeManip","list")
    return(timeManipL)
}
