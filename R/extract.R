#' extract
#'
#' extract
#'
#' @param date timeManip object
#' @param indice indices
#' @keywords timeManip
#' @export
#' @examples
#' a <- timeManip(fromPeriod="2013060205",toPeriod="2013062805",
#'                timeResolution="hourly",precision="hourly")
#' res <- extract(a,c(1,5,8))
#' str(res)
extract <- function(date, indice=NULL){
  if (!is.null(indice)) {
     res <- timeManip(fromPeriod     = YYYYmmddHHMMSS_chr(date$seqPeriod(indice[1])),
                      toPeriod       = YYYYmmddHHMMSS_chr(date$seqPeriod(indice[2])),
                      timeResolution = date$timeResolution(),
                      precision      = date$precision())
  } else res <- date
  return(res)
}
