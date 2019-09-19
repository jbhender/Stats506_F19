# Vectorization, Example 3
#
# In this example, we compare different ways of computing the correlation
# coefficient between a target variable "yvec" and each of a
# collection "xmat" of others.
#
# This example is based on Prof. Shedden's notes here: 
# http://dept.stat.lsa.umich.edu/~kshedden/Courses/Stat506/r_vectorization/
#
# Updated: September 19, 2019
# Author: James Henderson
# 80: --------------------------------------------------------------------------

# Simulation parameters: ------------------------------------------------------
n = 30
m = 10000
r = 0.4

# generate data: --------------------------------------------------------------
yvec = rnorm(n)
xmat = outer(array(1, m), yvec) # m x n
xmat = r * xmat + sqrt(1 - r^2) * rnorm(n * m) 

# Approach 1, explicit loop: ---------------------------------------------------

tm1 = proc.time()
r1 = NULL
for ( i in 1:m ) {
  r1[i] = cor(xmat[i, ], yvec)
}
tm1 = proc.time() - tm1

# Approach 2, using an apply function: -----------------------------------------
## Apply is a "functional" because it accepts a function as an argument.  
## function(v) cor(v, yvec) is an "anonymous" function. 

tm2 = proc.time()
r2 = apply(xmat, 1, function(v) cor(v, yvec) )
tm2 = proc.time() - tm2
all.equal(r1, r2)

# Approach 3, using linear algebra: --------------------------------------------
tm3 = proc.time()
rmn = rowMeans(xmat)
xmat_c = xmat - outer(rmn, array(1, n))
rsd = apply(xmat, 1, sd)
xmat_s = xmat_c / outer(rsd, array(1, n))
yvec_s = {yvec - mean(yvec)} / sd(yvec)
r3 = xmat_s %*% yvec_s / {n - 1}
r3 = as.vector(r3)
tm3 = proc.time() - tm3
all.equal(r1, r3)

# Approach 4, linear algebra with broadcasting: -------------------------------
tm4 = proc.time()
rmn = rowMeans(xmat)
xmat_c = xmat - rmn
rvar = rowSums(xmat_c^2) / {dim(xmat)[2] - 1}
rsd = sqrt(rvar)
xmat_s = xmat_c / rsd
yvec_s = {yvec - mean(yvec)} / sd(yvec)
r4 = xmat_s %*% yvec_s / {n - 1}
r4 = as.vector(r4)
tm4 = proc.time() - tm4
all.equal(r1, r4)

# Format and print the results: -----------------------------------------------
cat(
  sprintf("1: %5.3f s \n2: %5.3f s \n3: %5.3f s \n4: %5.3f s \n",
            tm1[3], tm2[3], tm3[3], tm4[3] 
  )
)

# 80: --------------------------------------------------------------------------
