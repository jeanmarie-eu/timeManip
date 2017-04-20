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
  i1 <- i2 <- NULL
  i1 <- which(ts_sub>=ts[1])
  i2 <- which(ts_sub<=ts[length(ts)])
  if (!is.null(i1) && !is.null(i2)) {
     indice <- seq(i1[1],i2[length(i2)])
  } else indice <- NULL
  return(indice)
}
