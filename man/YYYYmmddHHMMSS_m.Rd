% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_convert.R
\name{YYYYmmddHHMMSS_m}
\alias{YYYYmmddHHMMSS_m}
\title{Get YYYYmmddHHMMSS_m}
\usage{
YYYYmmddHHMMSS_m(date)
}
\arguments{
\item{date}{POSIXlt}
}
\description{
Get the year, month, day, hour, minute and second as a matrix
}
\examples{
res <- YYYYmmddHHMMSS_chr("20150101001034")
YYYYmmddHHMMSS_m(res)
}
\keyword{timeManip}
