% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/attach.r
\name{doc_attach}
\alias{doc_attach}
\title{Include an attachment either on an existing or new document.}
\usage{
doc_attach(cushion = "localhost", dbname, docid, attachment, attname, ...)
}
\arguments{
\item{cushion}{A cushion name}

\item{dbname}{Database name. (charcter)}

\item{docid}{Document ID (character)}

\item{attachment}{The attachment object name}

\item{attname}{Attachment name.}

\item{...}{Curl args passed on to \code{\link[httr]{GET}}}
}
\description{
Include an attachment either on an existing or new document.
}
\examples{
\donttest{
# put on to an existing document
doc <- '{"name":"guy","beer":"anybeerisfine"}'
doc_create(dbname="sofadb", doc=doc, docid="guysbeer")
myattachment <- "just a simple text string"
myattachment <- mtcars
doc_attach(dbname="sofadb", docid="guysbeer", attachment=myattachment, attname="mtcarstable.csv")
}
}

