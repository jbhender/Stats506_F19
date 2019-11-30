*------------------------------------------------------------------------------*
* sum_reg is a mata function for summarizing regression models
* 
* usage: mata sum_reg("name")
* 
* sum_reg creates a matrix (in Stata) with columns est, se, lwr, and upr
*  representing:
*    "est" =  the values in e(b), 
*    "se"  =  the square root of the diagonal of e(V)
*    "lwr" and "upr" = est +/- 1.96*se
*
* It will work with glm objects, mixed regression, and marigns -- but we rely
* on the user to know what the elements of e(b) are and how they are scaled.
* 
*------------------------------------------------------------------------------*
mata
 void sum_reg (string scalar name) {
   b  = st_matrix("e(b)")
   se = sqrt( diagonal( st_matrix("e(V)") ) )
   l = b' - 1.96 * se
   u = b' + 1.96 * se
   all = (b', se, l, u)
   st_matrix(name, all)
   st_matrixrowstripe(name, st_matrixcolstripe("e(b)") )
   cols = ("", "", "", "" \ "est", "se", "lwr", "upr")
   st_matrixcolstripe(name, cols')
  }
end
