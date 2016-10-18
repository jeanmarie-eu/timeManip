#' Keep the part of the date you want
#'
#' The function \code{keep()} extract the part of the date you want
#' @param fromPeriod "YYYYmmddHHMMSS"
#' @param toPeriod "YYYYmmddHHMMSS"
#' @param timeResolution choice between "daily","hourly","minute","second"
#' @param part either "year" "month", or "day"
#' @keywords timeManip
#' @export
#' @examples
#' keep()

keep <- function(fromPeriod="20160420",
		             toPeriod="20160630",
		             timeResolution="daily",
								 part="month"){

     seqPeriod <- dateTimeSerie(timeResolution=timeResolution,fromPeriod=fromPeriod,toPeriod=toPeriod)$seqPeriod
		 
     if (!inherits(seqPeriod, "dates")) seqPeriod <- chron::as.chron(seqPeriod)

		 res <- switch(part,
			        "year"  = chron::month.day.year(as.numeric(seqPeriod))$year,
							"month" = chron::month.day.year(as.numeric(seqPeriod))$month,
							"day" = chron::month.day.year(as.numeric(seqPeriod))$day,
							(message=paste0("part:", part," not taken into account "))
			 )

     return(res)
}
