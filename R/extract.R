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
#' res <- extract(a,indice=list(offset=3,count=5))
#' res$fromPeriod()
#' res$toPeriod()
extract <- function(date, indice=NULL){ #list(offset,count)
  if (!is.null(indice)) {
     res <- timeManip(fromPeriod     = YYYYmmddHHMMSS(date$seqPeriod(indice$offset)),
                      toPeriod       = YYYYmmddHHMMSS(date$seqPeriod((indice$offset+indice$count-1))),
                      timeResolution = date$timeResolution(),
                      precision      = date$precision())
  } else res <- date
  return(res)
}
