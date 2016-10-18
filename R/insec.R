#' In second
#'
#' The function \code{insec()} gives the values in second
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param v value
#' @keywords timeManip
#' @export
#' @examples
#' res <- insec(timeResolution="hourly",v=4)

insec  <- function(timeResolution,v=1){

  res <- switch(timeResolution,
         "daily"                  = 86400*v,
         "hourly"                 = 3600*v,
         "minute"                 = 60*v,
         "second"                 = 1*v,
         (message=paste0("Time resolution:", timeResolution," not taken into account "))
         )

   return(res)
}
