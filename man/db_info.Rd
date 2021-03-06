% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/db_info.r
\name{db_info}
\alias{db_info}
\title{List database info.}
\usage{
db_info(cushion = "localhost", dbname, as = "list", ...)
}
\arguments{
\item{cushion}{A cushion name}

\item{dbname}{Database name}

\item{as}{(character) One of list (default) or json}

\item{...}{Curl args passed on to \code{\link[httr]{GET}}}
}
\description{
List database info.
}
\examples{
\donttest{
# local databasees
db_info(dbname="sofadb")
db_info(dbname="sofadb", as='json')

# a database on cloudant or iriscouch
db_info("cloudant", "gaugesdb_ro")
db_info("iriscouch", "helloworld")
}
}

