##  data.table workshop 
##
## Scoping rules for j expressions during joins
## 
## James Henderson (jbhender)
## October 24, 2018

# data.table package: ---------------------------------------------------------
library(data.table)
rm( list = ls() )

# Example data.tables: --------------------------------------------------------
X = data.table(x = rep(c("a", "b"), c(2, 3)), y = 1:5)
Y = data.table(z = rep(c(TRUE, FALSE), c(2, 3)), x = rep('yy', 5), y = 0:4 )

# Joins: --------------------------------------------------------------------

# Use merge
?merge
merge(X, Y, by = 'y') # Inner
merge(X, Y, by = 'y', all.x = TRUE) # Left
merge(X, Y, by = 'y', all.y = TRUE) # Right
merge(X, Y, by = 'y', all.x = TRUE, all.y = TRUE) # Outer

## Construct an environment to compute in "j" using columns from two data.tables
# Left join
X[Y, .(y, x, z), on = 'y']
Y[X, .(y, x, z), on = 'y']

# Inner join
X[Y, .(y, x, z), on = 'y', nomatch = 0L]

# Scoping rules for joins: ----------------------------------------------------
m = 'global'; n = 'global'
list1 = list( m = 'list1', n = 'list1')
list2 = list( m = 'list2', n = 'list2')

setkey(X, 'y')
setkey(Y, 'y')

X[Y, .(x), verbose=TRUE]
Y[X, .(x)]

X[, .(m)]

f = function(X){
  m = 'm_from_f'
  X[ , .(m, n)]
}

with(list1, 
 within(list2, {f = f(X)})
)
