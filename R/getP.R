#' Get the part of the POSIXlt date you want
#'
#' Get the part of the POSIXlt date you want
#' @param date POSIXlt
#' @param part "year" "month", "day", "hour", "minute", "second"
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' getP(res,"month")
getP <- function(date,part){

     res <- switch(part,
			        "year"  = 1900+date$year,
							"month" = 1+date$mon,
							"day" = date$mday,
							"hour" = date$hour,
							"minute" = date$min,
							"second" = date$sec,
							"wday" = date$wday,
							"yday" = date$yday,
							"isdst" = date$isdst,
							"tzone" = attributes(date)$tzone,
							(message=paste0("part:", part," not taken into account "))
			 )

     return(res)
}
