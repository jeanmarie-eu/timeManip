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
    tmp <- contain(date$seqPeriod(),d$seqPeriod())
    indice_d <- tmp$indice_sub
    indice_date <- tmp$indice_main

    if (!is.null(indice_d)) {
      indiceT_d <- offsetCount(indice_d)
    } else indiceT_d <- NULL

    if (!is.null(indice_date)) {
      indiceT_date <- offsetCount(indice_date)
    } else indiceT_date <- NULL

    return(list(indice_sub = list(indiceT=indiceT_d),
                indice_main = list(indiceT = indiceT_date)))
  } else {
    return(list(indice_sub  = list( indiceT = list(offset=1,count=date$nbStep()) ),
                indice_main = list( indiceT = list(offset=1,count=date$nbStep()) )))
  }
}

offsetCount <- function(ind){
  offset = min(ind)
  count  = max(ind)-min(ind)+1
  return(list(offset=offset,count=count))
}
