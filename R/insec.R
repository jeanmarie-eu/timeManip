#' In second
#'
#' The function \code{insec()} gives the values in second
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param v value
#' @param INV boolean if into non-second
#' @keywords timeManip
#' @export
#' @examples
#' res <- insec(timeResolution="hourly",v=4)
#' res <- insec(timeResolution="hourly",v=4,INV=TRUE)
insec  <- function(timeResolution,v=1,INV=FALSE){
  if (!INV){
    res <- switch(timeResolution,
           "yearly"                 = NA,
           "monthly"                = NA,
           "daily"                  = 86400*v,
           "hourly"                 = 3600*v,
           "three-hourly"           = 3*3600*v,
           "minute"                 = 60*v,
           "second"                 = 1*v,
           stop("Time resolution:", timeResolution," not taken into account.")
           )
  } else if (INV) {
    res <- switch(timeResolution,
           "yearly"                 = v/(12*30*24*3600),
           "monthly"                = v/(30*24*3600),
           "daily"                  = v/(24*3600),
           "hourly"                 = v/3600,
           "three-hourly"           = v/(3*3600),
           "minute"                 = v/60,
           "second"                 = v,
           stop("Time resolution:", timeResolution," not taken into account.")
           )
  }

  return(res)
}
