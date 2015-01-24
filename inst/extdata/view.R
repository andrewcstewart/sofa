#!/usr/bin/env Rscript

# Implementation of a view server for functions written in Python.
suppressPackageStartupMessages(require(jsonlite))


functions = list()

# Command:  reset
# Arguments:	Query server state (optional)
# Returns:	true
# This resets the state of the Query Server and makes it forget all previous input. If applicable, this is 
# the point to run garbage collection.
reset <- function (config=NULL) {
  functions <<- list()
  return(TRUE)
}


# Command:  add_fun
# Arguments:	Map function source code.
# Returns:	true
# When creating or updating a view the Query Server gets sent the view function for evaluation. 
# The Query Server should parse, compile and evaluate the function it receives to make it callable later. 
# If this fails, the Query Server returns an error. CouchDB might store several functions before sending 
# in any actual documents.
add_fun <- function(str) { 
  fun <- eval(parse(text = str))   
  if(class(fun) != "function") {
    return(FALSE) # error
  }
  functions <<- c(functions, fun)
  return(TRUE)
}


# Command:  map_doc
# Arguments:	Document object
# Returns:	Array of key-value pairs per applied function
# When the view function is stored in the Query Server, CouchDB starts sending in all the documents in the 
# database, one at a time. The Query Server calls the previously stored functions one after another with a 
# document and stores its result. When all functions have been called, the result is returned as a JSON string.
map_doc <- function(doc) {
  results <- list()

  for (fun in functions) {
    result <- fun(doc)
    results <- c(results,list(result))
  }
  return(list(results))
}


# handlers <- list('reset'= reset, 'add_fun'= add_fun, 'map_doc'= map_doc, 'reduce'= reduce, 'rereduce'= rereduce)
handlers <- list('reset'= reset, 'add_fun'= add_fun, 'map_doc'= map_doc)

# "main"
f <- file("stdin")
# f <- file("~/Desktop/view.r")
open(f)
# while(length(line <- readLines(f,n=1)) > 0) {
for(line in readLines(f)) {
  cmd <- fromJSON(line)
  retval <- handlers[[cmd[[1]]]](cmd[[-1]])
  print(toJSON(retval, auto_unbox=TRUE, null="null"))
}
close(f)


############ TESTING ###########
# fpy <- file("~/Desktop/view.py")
# open(fpy)
# close(f)
# 
# a <- readLines(fpy)
# b <- fromJSON(a[2])
# 
# reval <- h[[b[1]]](b[2])
# 
# fun1 <- function(a) { return( paste("processing: ",a) ) }
# h <- list('fun1'=fun1)
# d <- c("fun1","foo","bar")
# h[[d[1]]](d[-1])





