## Problem Set 1, Question 3
## Stats 506, Fall 2018
##
## RECS consumption data is available at:
## https://www.eia.gov/consumption/residential/data/2015/
##
## Author: James Henderson
## Updated: September 29, 2018

# libraries: ------------------------------------------------------------------
library(tidyverse)

# Multiplier for confidence level: --------------------------------------------
m = qnorm(.975)

# data: -----------------------------------------------------------------------
file = './recs2015_public_v3.csv'
if ( !file.exists(file) ){
 recs =  readr::read_delim('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv',
                    delim = ',')
  readr::write_delim(recs, delim = ',')
} else {
  recs = readr::read_delim(file, delim = ',')
}

# Replicate weights, gathered into long fromat: -------------------------------
weights =  readr::read_delim('./recs2015_weights.csv.gz', delim=',')
weights_long = gather(weights, key = 'repl', value = 'w', BRRWT1:BRRWT96) %>%
  select(-NWEIGHT)

# Division map: ---------------------------------------------------------------
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)

############
## Part a ##
############
# Notes: Stucco construction as major outside wall material by Division.
## Stucco: WALLTYPE == 4

# Point estimate: ------------------------------------------------------------
p_stucco = 
  recs %>%
  transmute( DOEID, 
             division = factor(DIVISION, 1:10, divisions),
             NWEIGHT, 
             WALLTYPE
  ) %>%
  group_by(division) %>%
  summarize( p_stucco =  sum( NWEIGHT*{WALLTYPE == 4} ) / sum(NWEIGHT) )

# Estimates from replicate weights: -------------------------------------------
p_stucco_r = 
  weights_long %>%
  left_join(
    recs %>%
    transmute(DOEID, DIVISION, NWEIGHT, WALLTYPE) %>%
    mutate( division = factor(DIVISION, 1:10, divisions) ) ,
    by = 'DOEID'
  ) %>%
  group_by(division, repl) %>%
  summarize( r_stucco =  sum( w*{WALLTYPE == 4} ) / sum(w) )

# Compute standard errors: ----------------------------------------------------
p_stucco_r =
  p_stucco_r %>%
  left_join( p_stucco, by = 'division') 

p_stucco = 
  p_stucco_r %>%
  group_by(division) %>%
  summarize( p_stucco = p_stucco[1],
             se_stucco = 2 * sqrt( mean( {r_stucco - p_stucco}^2) )
  )
  
p_stucco = 
  p_stucco %>%
  mutate( lwr = pmax(p_stucco - m*se_stucco, 0), 
          upr = p_stucco + m*se_stucco
  )

# Repeat using a function: ---------------------------------------------------
est_recs = function(df, weights, fay = .5, m = qnorm(.975), 
                    ci_format = '%4.1f (%4.1f, %4.1f)'){
  # function to compute the weighted mean sum(w*x)/sum(w) in df
  # and it associated standard error using replicate weights in weights
  #
  # Args:
  #  weights: a tibble with columns: id, repl, w
  #  df: a (possibly grouped) tibble with columns: id, w, x
  #  m: a multipler for computing a confidence interval: xbar +/- m*se
  #
  # Details: The weighted sum of `x`` in `df`` is first computed and then its
  # standard error is found by joining against the replicate weights `weights`
  # and recomputing for each unique value of `repl`
  #
  # Returns: 
  #  A tibble with columns:
  #   - est: weighted mean of column "x"
  #   - se:  standard error determined by replicate weights and Fay coefficient
  #   - ci:  est +/- m*se
  
  # Point estimate
  pe = df %>% summarize(est = sum(.data$w * .data$x) / sum(.data$w) )
  
  # Replicate estimates
  pe_r = df %>% 
    select(-w) %>%
    left_join( weights, by = 'id' )  %>% 
    group_by( .data$repl, add=TRUE) %>%
    summarize( r = sum( .data$w * .data$x ) / sum(.data$w) )
  
  # Std error and confidence interval
  pe_r %>% 
    left_join(pe, by = as.character( groups(df) ) ) %>%
    summarize( est = est[1], 
               se = 1/fay * sqrt( mean( {r - est}^2 ) ),
               lwr = est - m*se,
               upr = est + m*se
    ) %>%
    mutate( ci = sprintf(ci_format, est, lwr, upr) )
}

# Test the function above gives the previous results: -------------------------
weights_long = weights_long %>% rename( id = DOEID )

if( FALSE ){
  df =  
    recs %>%
    transmute( id = DOEID, 
               division = factor(DIVISION, 1:10, divisions),
               w = NWEIGHT, 
               x = 100*{WALLTYPE == 4}
    ) %>%
    group_by( division )
  
  
  est_recs( df, weights_long )
}

############
## Part b ##
############
# Notes: What is the average total kwh of electricity usage by division?
# By each division Urban and rural subgroups?

# Point estimate: ------------------------------------------------------------
kwh = 
  recs %>%
  transmute( id = DOEID, w = NWEIGHT, x = KWH,
             division = factor(DIVISION, 1:10, divisions) 
  ) %>%
  group_by(division) 

kwh = est_recs(kwh, weights_long, ci_format = '%4.0f (%4.0f, %4.0f)')

kwh_div_urban = 
  recs %>%
  transmute( id = DOEID, w = NWEIGHT, x = KWH,
             division = factor(DIVISION, 1:10, divisions),
             urban = UATYP10 %in% c('U', 'C')
  ) %>%
  group_by(division, urban) 

kwh_div_urban = 
  est_recs( kwh_div_urban, weights_long, ci_format = '%4.0f (%4.0f, %4.0f)' )

############
## Part C ##
############

# Internet access data: -------------------------------------------------------
internet =  recs %>%
  transmute( id = DOEID, w = NWEIGHT, x = 100*INTERNET,
             division = factor(DIVISION, 1:10, divisions),
             urban = UATYP10 %in% c('U', 'C')
  ) %>%
  group_by(division, urban) 

# Urban/rural estimates for each division: -----------------------------------
internet_ru = est_recs(internet, weights_long) 

# Point estimate for difference: ---------------------------------------------
#! Could you replace spread and mutate with a group_by and mutate? 
pe = internet %>%
  summarize(est = sum(w*x) / sum(w) ) %>%
  spread(urban, est) %>%
  mutate(est = `TRUE` - `FALSE`) 

# Replicate estimates for difference: ----------------------------------------
pe_r = internet %>% 
  select(-w) %>%
  left_join( weights_long, by = 'id' )  %>% 
  group_by( repl, add=TRUE ) %>%
  summarize( r = sum(w*x) / sum(w) ) %>%
  spread(urban, r) %>%
  mutate(r = `TRUE` - `FALSE`) 

# Std error and confidence interval for differnce: ---------------------------
internet_disp = pe_r %>% 
  left_join(pe, by = c('division') ) %>%
  summarize( est = est[1], 
             se = 2 * sqrt( mean( {r - est}^2 ) )
  ) %>%
  mutate( ci = sprintf('%4.1f%% (%4.1f, %4.1f)', est, est - m*se, est + m*se)
  ) 

# Join urban & rural estimates to differences: -------------------------------
internet_disp = internet_disp %>%
  left_join( internet_ru %>% 
               select(division, urban, ci) %>%
               spread(urban, ci), 
             by='division'
  ) %>%
  rename(Rural = `FALSE`, Urban = `TRUE`, Diff = ci)
