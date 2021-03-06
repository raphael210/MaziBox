% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funset.R
\name{EE_Plot}
\alias{EE_Plot}
\title{Plug in TSErr object and return the summary plot}
\usage{
EE_Plot(TSErr, bmk = NULL)
}
\arguments{
\item{TSErr}{The TSErr object which must containing No and err columns.}
}
\value{
Plot
}
\description{
Plug in TSErr object and return the summary plot
}
\examples{
date <- as.Date(c("2014-07-01","2014-07-08"))
stockID <- c("EQ000001","EQ000002")
ETS <- data.frame(date, stockID)
TSErr <- EE_GetTSErr(ETS)
EE_Plot(TSErr)
}

