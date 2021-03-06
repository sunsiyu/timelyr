% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util-dates.R
\name{create_dates}
\alias{create_dates}
\title{Create dates}
\usage{
create_dates(start, end, by, n, method = c("sequence", "random")[1], ...)
}
\arguments{
\item{start}{start date}

\item{end}{end date}

\item{by}{if method is sequence, seq() argument by}

\item{n}{if method is random, vector of length n dates to create}

\item{method}{partial matching methods to be used}
}
\value{
a vector of dates
}
\description{
Create dates with different methods
}
\examples{
d1 <- create_dates("2015-01-01", "2015-04-01", by="1 week")
d2 <- create_dates("2015-01-01", "2015-04-01", n = 10, method = "ran")
}

