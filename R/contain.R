#' contain
#'
#' contain
#'
#' @param a POSIXlt timeserie
#' @param b POSIXlt timeserie
#' @keywords timeManip
#' @export
#' @examples
#' \dontrun{
#' contain()
#' }
contain  <- function(a,b){
  i1 <- which(b>=a[1])
  i2 <- which(b<=a[length(a)])
  indice <- seq(i1[1],i2[length(i2)])
  return(indice)
}
