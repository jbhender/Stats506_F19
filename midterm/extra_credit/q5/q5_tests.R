# Tests for Question 5 from the Midterm Extra Credit
# Stats 506, Fall 2019
# Updated: October 20, 2019

# source the script defining the about generic and associated methods
source('./midterm_ec_q5.R')

# open a connection to a file, to capture the concole output
sink('./q5_tests.txt')

# test the default and method dispatch
cat('Test 1: \n')
set.seed(42)
x = rnorm(1:5)
names(x) = letters[1:5]
about(x)
cat('\n')

# test the data.frame method
cat('Test 2: \n')
x = data.frame( 
  a = LETTERS[1:5], 
  b = 1:5, 
  c = factor(letters[1:5], letters[1:5]),
  d = rnorm(5),
  e = as.Date(1:5, origin = Sys.Date())
)
about(x)
cat('\n')

# test for part d, the method for tibbles
cat('Test 3: \n')
x = tibble::as_tibble(x) %>% mutate( f = lapply(x, names) )
about(x)
cat('\n')

# close the file we are writing output to
sink()
