% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funset.R
\name{EE_Analyzer}
\alias{EE_Analyzer}
\title{Wrap up ETS analyzing functions}
\usage{
EE_Analyzer(ets, db = c("EE_CroxSecReg", "pct_chg"), win1 = 20, win2 = 30)
}
\arguments{
\item{db}{The database of studying objects. Whether to study residuals or daily_pct_chg.}

\item{win1}{The time window(days) before the events happened.}

\item{win2}{The time window(days) after the events happened.}
}
\value{
list. Containing stat table, day0 analyzing, daily perfomance and plots.
}
\description{
Wrap up ETS analyzing functions
}
\examples{
ets <- ets.employeeplan()
EE_Analyzer(ets)
}

