% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/uuids.r
\name{uuids}
\alias{uuids}
\title{Get uuids.}
\usage{
uuids(cushion = "localhost", count = 1, as = "list", ...)
}
\arguments{
\item{cushion}{A cushion name}

\item{count}{(numeric) Number of uuids to return. Default: 1}

\item{as}{(character) One of list (default) or json}

\item{...}{Curl args passed on to \code{\link[httr]{GET}}}
}
\description{
Get uuids.
}
\examples{
\donttest{
uuids()
uuids(as = 'json')
uuids("cloudant")
uuids("iriscouch")
}
}

