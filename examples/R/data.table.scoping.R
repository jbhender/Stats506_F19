## Notes on how variable names are scoped when using on-the-fly joins in
## data.table. 
##
## Scoping rules for j expressions during on-the-fly joins
## 
## Updated: November 12, 2019
## Author: James Henderson

# data.table package: ---------------------------------------------------------
library(data.table)
rm( list = ls() )

# Example data.tables: --------------------------------------------------------
X = data.table(x = rep(c("a", "b"), c(2, 3)), y = 1:5)
Y = data.table(z = rep(c(TRUE, FALSE), c(2, 3)), x = rep('yy', 5), y = 0:4 )

# Joins: --------------------------------------------------------------------

# Use merge
#?merge
merge(X, Y, by = 'y') # Inner
merge(X, Y, by = 'y', all.x = TRUE) # Left
merge(X, Y, by = 'y', all.y = TRUE) # Right
merge(X, Y, by = 'y', all.x = TRUE, all.y = TRUE) # Outer

## Construct an environment to compute in "j" using columns from two data.tables

# Left join
X[Y, on = 'y']  # All rows in X and all columns from X and Y, Y dups are i.XXX      
X[Y, .(y, x, z), on = 'y']   # Dup names refers to X first
X[Y, .(y, x, i.x), on = 'y'] # `i.` means "from the DT in the i position"
Y[X, .(y, x, z), on = 'y']   # swap left/right for precedence

# Inner join
X[Y, .(y, x, z), on = 'y', nomatch = 0L]

# Keyed joins: ----------------------------------------------------------------
## Key X and Y by "y"
setkey(X, 'y')
setkey(Y, 'y')

X[Y, .(x), verbose=TRUE]
Y[X, .(x)]

# Scoping rules for joins: ----------------------------------------------------

## Two globals
m = 'global'; n = 'global'

## Two lists with names matching globals
list1 = list( m = 'list1', n = 'list1')
list2 = list( m = 'list2', n = 'list2')

# Note, m refers to a global, since it is not a column in X
X[, .(m)]


# Here m is redefined in the scope of f, but n is still only global
f = function(X){
  m = 'm_from_f'
  X[ , .(m, n)]
}
f(X) 


# Now, call f from within list2, evaluated in the "list1" environment.

## m and n are evaluated in the calling environment
with(list1, 
 X[ , .(m, n)]
)

new = within(list2, {}) # to undestand within
within(list2, { l2 = X[ , .(m, n)]})

## functions jump up to the defining environment
with(list1, 
 within(list2, {f = f(X)})
)


