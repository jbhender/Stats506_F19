# Vectorization, Example 2 
#
# Compare loops and vectorized approaches to computing row variances.
#
# This script illustrates two core ideas for vectorization: 
#    1. Using built-in vectorized functions like rowMeans
#    2. Using R's broadcasting rules to advantage. 
#
# Updated: September 18, 2019
# Author: James Henderson
# 80: --------------------------------------------------------------------------

# simulation parameters: -------------------------------------------------------
m = 100000
n = 30
xmat = rnorm( m * n )
dim(xmat) = c(m, n)

# approach 1, using apply: -----------------------------------------------------
## This is comparable to a well-written for loop.  
tm1 = proc.time()
rowvar1 = apply(xmat, 1, var)
tm1 = proc.time() - tm1

# approach 2, vectorize using rowMeans: ----------------------------------------
tm2 = proc.time()
rowvar2 = 
  rowMeans( {xmat - rowMeans(xmat)}^2 ) * dim(xmat)[2] / {dim(xmat)[2] - 1} 
tm2 = proc.time() - tm2

## Check that all versions are equal: ------------------------------------------
all.equal(rowvar1, rowvar2)

## Report on the difference: ---------------------------------------------------
cat("Apply: ", tm1[3], "s, Vectorized: ", tm2[3], "s, Ratio: ",
    round( tm1[3] / tm2[3], 1), '.\n', sep = '' )

# approach 3, using a well-written for loop: -----------------------------------
tm3 = proc.time()
rowvar3 = vector( mode = 'numeric', length = nrow(xmat) )
for ( i in 1:nrow(xmat) ) {
  rowvar3[i] = var(xmat[i, ])
}
tm3 = proc.time() - tm3

## Check that this approach is equal
all.equal(rowvar4, rowvar0)

# approach 4, using a poorly written for loop: ---------------------------------
tm4 = proc.time()
rowvar4 = c()
for ( i in 1:nrow(xmat) ) {
  x = xmat[i, ]
  rowvar4[i] = var(x)
}
tm4 = proc.time() - tm4

## Check that this approach is equal
all.equal(rowvar4, rowvar0)

