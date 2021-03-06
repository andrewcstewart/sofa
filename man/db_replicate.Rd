% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/db_replicate.r
\name{db_replicate}
\alias{db_replicate}
\title{Upload (replicate) a local database to a remote database server, e.g., Cloudant, Iriscouch}
\usage{
db_replicate(from = "localhost", to = "cloudant", dbname,
  createdb = FALSE, as = "list", ...)
}
\arguments{
\item{from}{Couch to replicate from, Default: localhost}

\item{to}{Remote service name to upload to. One of cloudant, iriscouch.}

\item{dbname}{Database name.}

\item{createdb}{If TRUE, the function creates the db on the remote server before
uploading. The db has to exist before uploading, so either you do it separately
or this fxn can do it for you. Default = FALSE}

\item{as}{(character) One of list (default) or json}

\item{...}{Curl args passed on to \code{\link[httr]{GET}}}
}
\description{
Upload (replicate) a local database to a remote database server, e.g., Cloudant, Iriscouch
}
\examples{
\donttest{
# Create a database locally
db_list()
db_create('hello_earth')

# replicate to a remote server
db_replicate(to="cloudant", dbname="hello_earth", createdb=TRUE)
changes("cloudant", dbname = "hello_earth")
doc_create("cloudant", dbname = "hello_earth", doc = '{"language":"python","library":"requests"}')
changes("cloudant", dbname = "hello_earth")

doc_create("cloudant", dbname = "hello_earth", doc = '{"language":"R"}', docid="R_rules")
doc_get("cloudant", dbname = "hello_earth", docid='R_rules')

db_delete('cloudant', 'hello_earth')
}
}

