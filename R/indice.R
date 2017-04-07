#' indice_subtemporal
#'
#' indice_subtemporal
#' @param date timeManip object
#' @param d sub timeManip object
#' @keywords timeManip
#' @export
#' @examples
#' a <- timeManip(fromPeriod="2013060205",toPeriod="2013060605",
#'                timeResolution="hourly",precision="hourly")
#' b <- timeManip(fromPeriod="2013060305",toPeriod="2013060505",
#'                timeResolution="hourly",precision="hourly")
#' res <- indice_subtemporal(a,b)
#' str(res)
indice_subtemporal <- function(date,d) {
  if (!identical(date,d)){
    indice_temporal <- timeManip::contain(d$seqPeriod(),date$seqPeriod())
    indiceT <- offsetCount(indice_temporal)
    return(list(indiceT = indiceT))
  } else {
    return(list(indiceT = list(offset=1,count=date$nbStep())))
  }
}

offsetCount <- function(ind){
  offset = min(ind)
  count  = max(ind)-min(ind)+1
  return(list(offset=offset,count=count))
}
