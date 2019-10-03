# This script provides examples illustrating S3 class inheritance and method
# dispatch using an example taken from Advanced R, Chapter 13:
#
# Date: October 3, 2019
# Author: James Henderson

## Create a class "secret": ---------------------------------------------------

### the constructor
new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "secret")
}

### a print method
print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

x <- new_secret(c(15, 1, 456))

### The problem, subsetting doesn't retain the class
x[1]
print( x[1] )
print.secret( x[1] )
class(x[1])

### an inefficient solution 
`[.secret` = function(x, i){
  x = unclass(x)  
  new_secret(x[i])
}

x[1]
print( x[1] )
print.secret( x[1] )

### a better solution using NextMehod
`[.secret` = function(x, i){
  new_secret( NextMethod() )
}
x[1]
print( x[1] )
print.secret( x[1] )

