
#' Get the year
#'
#' Get the year
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' year(res)
year <- function(date,CHAR=FALSE){
  return(getP(date,"year",CHAR=CHAR))
}

#' Get the month
#'
#' Get the month
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' month(res)
month <- function(date,CHAR=FALSE){
  return(getP(date,"month",CHAR=CHAR))
}

#' Get the day
#'
#' Get the day
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' month(res)
day <- function(date,CHAR=FALSE){
  return(getP(date,"day",CHAR=CHAR))
}

#' Get the hour
#'
#' Get the hour
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' hour(res)
hour <- function(date,CHAR=FALSE){
  return(getP(date,"hour",CHAR=CHAR))
}

#' Get the minute
#'
#' Get the minute
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' minute(res)
minute <- function(date,CHAR=FALSE){
  return(getP(date,"minute",CHAR=CHAR))
}

#' Get the second
#'
#' Get the second
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' second(res)
second <- function(date,CHAR=FALSE){
  return(getP(date,"second",CHAR=CHAR))
}

#' Get the wday
#'
#' Get the wday
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' wday(res)
wday <- function(date,CHAR=FALSE){
  return(getP(date,"wday",CHAR=CHAR))
}


#' Get the yday
#'
#' Get the yday
#' @param date POSIXlt
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' yday(res)
yday <- function(date,CHAR=FALSE){
  return(getP(date,"yday",CHAR=CHAR))
}

#' Get the idist
#'
#' Get the idist
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' idist(res)
idist <- function(date){
  return(getP(date,"idist"))
}

#' Get the tzone
#'
#' Get the tzone
#' @param date POSIXlt
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' tzone(res)
tzone <- function(date){
  return(getP(date,"tzone"))
}


#' Get the part of the POSIXlt date you want
#'
#' Get the part of the POSIXlt date you want
#' @param date POSIXlt
#' @param part "year" "month", "day", "hour", "minute", "second"
#' @param CHAR boolean as character
#' @keywords timeManip
#' @export
#' @examples
#' res <- char2POSIXlt("YYYYmmddHH","2015010100")
#' getP(res,"month")
getP <- function(date,part,CHAR=FALSE){

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
      if ((CHAR) && is.numeric(res)) {
        if (res<10) res <- paste0("0",as.character(res))
      }

     return(res)
}
