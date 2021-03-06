% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/revisions.r
\name{revisions}
\alias{revisions}
\title{Get document revisions.}
\usage{
revisions(cushion = "localhost", dbname, docid, simplify = TRUE,
  as = "list", ...)
}
\arguments{
\item{cushion}{A cushion name}

\item{dbname}{Database name}

\item{docid}{Document ID}

\item{simplify}{(logical) Simplify to character vector of revision ids. If FALSE, gives back
availabilit info too.}

\item{as}{(character) One of list (default) or json}

\item{...}{Curl args passed on to \code{\link[httr]{GET}}}
}
\description{
Get document revisions.
}
\examples{
\donttest{
revisions(dbname="sofadb", docid="a_beer")
revisions(dbname="sofadb", docid="a_beer", simplify=FALSE)
revisions(dbname="sofadb", docid="a_beer", as='json')
revisions(dbname="sofadb", docid="a_beer", simplify=FALSE, as='json')
}
}

