## Solution to Question 5
## Stats 506, Fall 2019

# a, define the about generic: ------------------------------------------------
about = function(x, ...) {
  UseMethod("about")
}

# b, define a default method using str: ---------------------------------------
about.default = function(x) {
  str(x, give.attr = FALSE)
}
# test for default method

# c, define a method for class data.frame: ------------------------------------
about.data.frame = function(x) {
  stopifnot( is.data.frame(x) )
  
  # i, basic information
  str1 = sprintf("'%s': %i obs of %i variables\n", 
                 class(x)[1], nrow(x), ncol(x) )
  cat(str1)

  # ii, number and names of numeric columns
  xnum = vapply(x, is.numeric, FUN.VALUE = FALSE)
  if ( any(xnum) ) {
    xvars = paste(names(x)[xnum], collapse = ', ')
    str2 = sprintf(" %i numeric variable(s): %s\n", sum(xnum), xvars)
    cat(str2)    
  }
  
  # iii, character and factor variables
  xcat = vapply(x, is.character, FUN.VALUE = FALSE) |
         vapply(x, is.factor, FUN.VALUE = FALSE)
  
  if ( any(xcat) ) {
    xvars = paste(names(x)[xcat], collapse = ', ')
    str2 = sprintf(" %i categorical variable(s): %s\n", sum(xcat), xvars)
    cat(str2)    
  }
  
}
# test for about.data.frame
x = data.frame( 
  a = LETTERS[1:5], 
  b = 1:5, 
  c = factor(letters[1:5], letters[1:5]),
  d = rnorm(5),
  e = as.Date(1:5, origin = Sys.Date())
)
about(x)

# d, define a method for class 'tbl': ----------------------------------------
about.tbl = function(x) {
  stopifnot( tibble::is_tibble(x) )
  
  NextMethod(x) # also ok to call about.data.frame directly, but not the generic
  #about.data.frame(x)

  # list columns
  xlist = vapply(x, is.list, FUN.VALUE = FALSE) 
  if ( any(xlist) ) {
    xvars = paste(names(x)[xlist], collapse=', ')
    str1 = sprintf(' %i list column(s): %s\n', sum(xlist), xvars)
    cat(str1)
  }  
  
}
# test for about.tbl
x = as_tibble(x) %>% mutate( f = lapply(x, names) )
about(x)
