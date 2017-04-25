#' contain
#'
#' contain
#'
#' @param ts POSIXlt timeserie
#' @param ts_sub POSIXlt sub timeserie
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' contain()
#' }
contain  <- function(ts,ts_sub){

  indice_sub  <- indice_sub(ts=ts,ts_sub=ts_sub)
  indice_main <- indice_main(ts=ts,ts_sub=ts_sub,indice_sub=indice_sub)
  return(list(indice_sub=indice_sub,indice_main=indice_main))
}

indice_sub <- function(ts,ts_sub){
  indice_sub <- i1_sub <- i2_sub <- NULL
  i1_sub <- which(ts_sub>=ts[1])
  i2_sub <- which(ts_sub<=ts[length(ts)])
  if (!is.null(i1_sub) && !is.null(i2_sub)) {
     indice_sub <- seq(i1_sub[1],i2_sub[length(i2_sub)])
  }
  return(indice_sub)
}

indice_main <- function(ts,ts_sub,indice_sub=NULL){
  indice_main <- i1_main <- i2_main <- NULL
  if (!is.null(indice_sub)) {
    i1_main <- which(ts == ts_sub[indice_sub[1]])
    i2_main <- which(ts == ts_sub[indice_sub[length(indice_sub)]])
    if (length(i1_main) && length(i2_main)) {
       indice_main <- seq(i1_main[1],i2_main[length(i2_main)])
    }
  }
  return(indice_main)
}
