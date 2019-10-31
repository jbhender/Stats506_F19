*------------------------------------------------------------------------------*
* Loop examples in Stata
* Stats 506, F19
*
* Updated: October 31, 2019
*------------------------------------------------------------------------------*

// Num list
foreach i of numlist 1/5 {
  display `i'
}

// varlist 
sysuse auto, clear
foreach var of varlist weight displacement gear_ratio {
  quietly regress mpg `var'
  local r2 = e(r2)
  display "The R^2 for `var' is ",, %4.2f `r2'
}

// alternate varlist
local myvars "weight displacement gear_ratio"
foreach var of varlist `myvars' {
  quietly regress mpg `var'
  local r2 = e(r2)
  display "The R^2 for `var' is ",, %4.2f `r2'
}

// use in for a general list
foreach var in weight displacement gear_ratio {
  quietly regress mpg `var'
  local r2 = e(r2)
  display "The R^2 for `var' is ",, %4.2f `r2'
}

// can specify that myvars is a local
local myvars "weight displacement gear_ratio"
foreach var of local myvars {
  quietly regress mpg `var'
  local r2 = e(r2)
  display "The R^2 for `var' is ",, %4.2f `r2'
}

// varlist versus in
foreach x in mpg weight-turn {
 display "`x'"
}

foreach x of varlist mpg weight-turn {
 display "`x'"
}
