% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rNetVISOR.R
\name{hsvtohex}
\alias{hsvtohex}
\title{Convert HSV Color values to Hex Color}
\usage{
hsvtohex(k)
}
\arguments{
\item{takes}{a row number (of the data frame that contains Hue, Saturation, and Value in columns 6, 7, and 8 respectively) as input.}
}
\description{
This function supports conversion of colors from the HSV scale to Hex scale. This function is a data.frame specific helper (wrapper) function that uses the built-in hsv function under the hood.
}
\examples{
hsvtohex()
}
\keyword{color,}
\keyword{hex,}
\keyword{hsv}

