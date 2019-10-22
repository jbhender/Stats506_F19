# Stats 506, Fall 2019
# Problem Set 2, Question 2
#
# This script contains functions for translating a set of points in the plane
# to begin at the origin and end with the secant along the positive x-axis.
# It also contains functions to compute summary measures describing the 
# efficiency of this path. 
#
# Author: James Henderson (jbhender@umich.edu)
# Date: October 2, 2019
#80: ---------------------------------------------------------------------------

# (a) translate so t0 is at origin: --------------------------------------------
trans_to_origin = function(xyt){
  # This function translates an n x 3 matrix of triples, xyt, so the first
  # point is at the origin. We assume the matrix is already ordered by t.
  # 
  # inputs: xyt - a numeric matrix with three columns
  # outputs: the translated matrix
  
  cbind(  xyt[, 1] - xyt[1, 1], xyt[,2] - xyt[1, 2], xyt[ ,3] - xyt[1, 3])  
}

# unit test for trans_to_origin: -----------------------------------------------
stopifnot( all( trans_to_origin( cbind(rnorm(3), rnorm(3), 1:3) )[1,] == 0 ) )

# (b) find angle between a point and the origin: -------------------------------
compute_angle = function(xy){
  # compute the angle in radians a point (x, y) forms with the origin and the
  #   x axis. 
  # inputs: xyt a length 2 numeric vector c(x, y)
  # outputs: an angle, in radians, between [-pi/2, pi/2] 
  # Note: an angle between [0, 2*pi] would make this and the next part easier.
  if ( xy[1] == 0 ) {
    # split special case when x = 0
    return( sign(xy[2]) * pi / 2 )
  } else if ( xy[1] > 0 ) {
    # x > 0
    return( sign(xy[2]) * atan( abs( xy[2] / xy[1] ) ) )
  } else {
    # x < 0
    return( sign(xy[2]) * {pi - atan( abs( xy[2] / xy[1] ) )} )
  }
}

# unit tests for compute_angle :----------------------------------------------
.eps = sqrt(.Machine$double.eps)
stopifnot( abs(compute_angle(c( 0,  1 )) - .5  * pi) < .eps ) 
stopifnot( abs(compute_angle(c( 0, -1 )) + .5  * pi) < .eps ) 
stopifnot( abs(compute_angle(c( 0.5,  0.5)) - .25 * pi) < .eps )
stopifnot( abs(compute_angle(c(-0.5,  0.5)) - .75 * pi) < .eps ) 
stopifnot( abs(compute_angle(c( 0.5, -0.5)) + .25 * pi) < .eps )
stopifnot( abs(compute_angle(c(-0.5, -0.5)) + .75 * pi) < .eps ) 
rm(.eps)

# (c) rotation function: ------------------------------------------------------
rotate = function(xyt, theta = 0, clockwise = FALSE) {
  # function to rotate the first 2-columns of a coordinate matrix xyt 
  # by theta radians.
  # inputs: xyt - a three column numeric matrix
  #         theta - the angle to rotate xyt[,1:2] 
  #         clockwise - rotate clockwise (defaults to false)
  
  if ( clockwise ) {
    s = 1
  } else {
    s = -1
  }
  R = matrix( c(cos(theta), -s*sin(theta), s*sin(theta), cos(theta) ), 2, 2 )  
  
  ## Apply the rotation
  cbind( xyt[ , 1:2] %*% R, xyt[ , 3] )
}
# unit tests for the rotation: ------------------------------------------------
.test_rotate = function(){
  .eps = sqrt( .Machine$double.eps )
  x = .5 * c(1, -1, -1,  1)
  y = .5 * c(1,  1, -1, -1)
  
  for ( i in 1:4 ) {
    xyt = cbind( c(0, x[i]), c(0, y[i]), 1:length(x) )
    theta = compute_angle(xyt[2, 1:2])
    stopifnot( abs( rotate(xyt, theta)[2, 2] ) < .eps ) 
  }
  
}
.test_rotate()
rm(.test_rotate)

# (d) combine the above into a single function for normalizing a trajectory: ---
normalize_traj = function(xyt) {
  # This function normalizes the trajectory in xyt to start at the origin and
  # conclude along the positive x-axis.
  #
  # Inputs: 
  #   xyt - an n x 3 numeric matrix with time in the final column
  # Output:
  #   A matrix with the same dimensions as xyt representing the normalized
  #   trajectory.  
  
  # check input
  stopifnot( is.numeric(xyt) ) 
  stopifnot( ncol(xyt) == 3 )

  # Compute the rotation agnle
  theta = compute_angle( xyt[nrow(xyt), 1:2] - xyt[1, 1:2] )
  
  # Translate and rotate to normalize
  rotate( trans_to_origin(xyt), theta = theta )
}
# Unit tests for normalize_traj: -----------------------------------------------
.test_normalize_traj = function() {
  n = 10
  tm = seq(12, 24, length.out = n)
  x = rnorm(n)
  y = rnorm(n)
  xyt = cbind(cumsum(x), cumsum(y), tm)
  
  xyt0 = normalize_traj(xyt)

  stopifnot( all( xyt0[1, ] == 0 ) )
  stopifnot( abs( xyt0[n, 2] ) < sqrt( .Machine$double.eps ) )
}
.test_normalize_traj()
rm(.test_normalize_traj)

# (e) compute measures: --------------------------------------------------------
## total distance function
comp_dist = function(x, y) {
  # compute the total distance between successive (x, y) pairs
  # inputs: x, y - numeric vectors of the same length
  # output: a numeric constant with the distance traveled along the trajectory 
  #         (x, y)
  stopifnot( length(x) == length(y) )
  sum( sqrt( diff( x )^2 + diff( y )^2 ) ) 
}

## area under the curve integrated using trapezoidal rule
comp_auc = function(x, y, absy = TRUE){
  # compute the area under the curve traced out by x, y
  # allowing cancellation in x and (optionally) y. 
  # inputs: x, y - numeric vectors of the same length
  #         abs - should we use the absolute value of y or allow cancellation?
  # output: .5 * sum_i (|y_i| + |y_i+1|)*(x_i+1 - x_i)
  stopifnot( length(x) == length(y) )
  
  dx = diff(x)
  if ( absy ) {
    return( .5 * sum( {abs( y[-length(y)] ) + abs( y[-1] )} * dx ) )
  } else {
    return( .5 * sum( {y[-length(y)] +  y[-1]} * dx ))
  }

}

comp_measures = function(xyt) {
  # This function computes the following curvature measures for a normalized
  # trajectory xyt given as an n x 3 numeric matrix. Each row  of xyt should 
  # give the coordinates in the the x, y at time t (in that order).
  # If the trajectory is not normalized, this function will error with reference
  # to 
  
  # Inputs: 
  #  xyt - a numeric matrix with three columns
  # Output: 
  #  A named vector with the following curvature measures:
  #    dist - the total (Euclidean / L2) distance traveled along the trajectory
  #    max_abs_dev - the maximum absolute deviation from the secant line 
  #                  representing a direct path from the first to last point.
  #    avg_abs_dev - the average abosolute deviation form the secant line. 
  #    auc - the absolute area under the curve, computed with the trapezoidal rule
  #          and allowing cancelation in the "x" dimension. 

  # Test the input  
  stopifnot( is.numeric(xyt) )
  stopifnot( ncol(xyt)  == 3 )
  if ( !all( xyt[1, ] == 0 ) || 
       {abs( xyt[nrow(xyt), 2] ) > sqrt( .Machine$double.eps )} ) {
    stop("Trajectory is not normalized. Use normalize_traj() first.\n")  
  }

  # Compute the curvature measures and return them
  c(
    'tot_dist' = comp_dist(xyt[, 1], xyt[, 2]),
    'max_abs_dev' = max( abs(xyt[, 2]) ),
    'avg_abs_dev' = mean( abs(xyt[, 2]) ),
    'AUC' = comp_auc(xyt[, 1], xyt[, 2])
  )
  
}

## Formats output of comp_measures() as a tibble
convert_vec2tibble = function(x) as_tibble( as.list(x) )

## Example usage: -------------------------------------------------------------
if ( FALSE ) {
  test_meas = 
    test_traj %>% 
    group_by(subject_nr, count_trial) %>%
    do( convert_vec2tibble( 
      comp_measures( normalize_traj( cbind(.$xpos, .$ypos, .$tm ) ) )
    ) 
    )
}


      
