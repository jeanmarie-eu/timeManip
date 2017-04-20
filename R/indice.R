#' indice_subdate
#'
#' indice_subdate
#' @param date timeManip object
#' @param d sub timeManip object
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' indice_subdate()
#' }
indice_subdate <- function(date,d) {
  if (!identical(date,d)){
    indice_temporal <- contain(date$seqPeriod(),d$seqPeriod())
    if (!is.null(indice_temporal)) {
      indiceT <- offsetCount(indice_temporal)
    } else indiceT <- NULL
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
