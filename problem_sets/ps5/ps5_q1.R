# Problem Set 5, Question 1
# Stats 506, Fall 2019
#
# This script estimates the disparity in internet acccess between urban and 
# rural areas by census division, using the RECS 2015 data:
#
#  https://www.eia.gov/consumption/residential/data/2015/csv/
#    recs2015_public_v4.csv
#
# This repeats the analysis done in Problem Set 4, Question 2 using Stata.
#
# Author: James Henderson
# Date: December 7, 2019
# 80: --------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(tidyverse); library(data.table)

# data: ------------------------------------------------------------------------
recs = fread('./recs2015_public_v4.csv')

# point estimates: -------------------------------------------------------------

## treat urban and urban cluster as the "urban"
recs[ , urban := ifelse(UATYP10 %in% c("U", "C"), 'urban', 'rural')]
# recs[,.N,.(UATYP10, urban)] ## check mapping

## proportion of homes with internet in each group
prop_internet_ur = 
  recs[ , .(prop_internet = sum(INTERNET * NWEIGHT) / sum(NWEIGHT)),
        keyby = .(DIVISION, urban)]

## point estimate of disparity
disp_internet_ur = 
  prop_internet_ur[ , .(delta_internet = diff(prop_internet)), DIVISION]

# pivot replicate weights to a long form for standard error computations: ------
recs_weights = melt(recs, 
                    id.vars = 'DOEID', 
                    measure.vars = grep('^BRR', names(recs), value = TRUE),
                    value.name = 'w',
                    variable.name = 'repl'
)

# compute prop internet for each set of replicate weights: ---------------------
prop_internet_ur_repl = 
  merge(recs_weights, 
        recs[,.(DOEID, INTERNET, DIVISION, urban)],
        by = 'DOEID') %>%
  .[ , .(prop_internet_r = sum(INTERNET * w) / sum(w)), 
     keyby = .(DIVISION, repl, urban)]

## replicate disparities
disp_internet_ur_repl = 
  prop_internet_ur_repl[ , .(delta_internet_r = diff(prop_internet_r)), 
                         .(DIVISION, repl)]

# compute standard errors using point and replicate estimates: -----------------
prop_internet_ur = 
  merge( prop_internet_ur, 
         prop_internet_ur_repl[, !"repl"], 
         by = c('DIVISION', 'urban') 
  ) %>%
  .[ , .( prop_internet = prop_internet[1],
          se = 2 * sqrt(mean({prop_internet_r - prop_internet}^2))
         ), .(DIVISION, urban) ]  

disp_internet_ur = 
  merge( disp_internet_ur, 
         disp_internet_ur_repl[, !"repl"], 
         by = c('DIVISION') 
  ) %>%
  .[ , .( delta_internet = delta_internet[1],
          se = 2 * sqrt(mean({delta_internet_r - delta_internet}^2)) 
        ), .(DIVISION) ]  

# construct CI bounds and drop se using reference semantics: -------------------
m = qnorm(.975)

prop_internet_ur[ , `:=`(lwr = prop_internet - m * se,
                         upr = prop_internet + m * se,
                         se = NULL)]

disp_internet_ur[ , `:=`(lwr = delta_internet - m * se,
                         upr = delta_internet + m * se,
                         se = NULL)]

# make plots/tables more interpretable by ordering by disparity: ---------------
divisions = c(
  "New England",
  "Middle Atlantic",
  "East North Central",
  "West North Central",
  "South Atlantic",
  "East South Central",
  "West South Central",
  "Mountain North",
  "Mountain South",
  "Pacific")
div_ord = disp_internet_ur[, order(delta_internet)]

prop_internet_ur[ , division := factor(DIVISION, div_ord, 
                                       labels = divisions[div_ord])]
setkey(prop_internet_ur, division)

disp_internet_ur[ , division := factor(DIVISION, div_ord, 
                                       labels = divisions[div_ord])]
setkey(disp_internet_ur, division)

# Make a table with all three pieces of information: --------------------------
tab1 =
  prop_internet_ur[ , .(division, urban,
                      ci = sprintf('%4.1f (%4.1f, %4.1f)', 
                                   100 * prop_internet, 100 * lwr, 100 * upr) )
 ] %>%
 dcast(., division ~ urban, value.var = 'ci' )

tab1 = 
  disp_internet_ur[ , 
   .(division, disparity = sprintf('%4.1f (%4.1f, %4.1f)', 
                             100 * delta_internet, 100 * lwr, 100 * upr) ) 
   ] %>%
   merge( tab1, ., by = 'division') 

# plots urban / rural internet proportions: ------------------------------------
p1 = 
  prop_internet_ur %>%
  rename( rurality = urban ) %>%
  ggplot( aes( x = division, color = rurality) ) +
  geom_point( aes(y = prop_internet), position = position_dodge(width = 0.5) ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr), 
                 position = position_dodge(width = 0.5) ) +
  theme_bw() +
  scale_color_manual( values = c("darkred", "darkblue")) +
  ylab("Proportion of homes with internet access.") +
  xlab("Census Division") +
  coord_flip() 

# plot the disparity: ----------------------------------------------------------
p2 = 
  disp_internet_ur %>%
  ggplot( aes( x = division)  ) +
  geom_point( aes(y = delta_internet) ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr) ) +
  theme_bw() +
  scale_color_manual( values = c("darkred", "darkblue")) +
  ylab("Difference in proportion of homes with internet access.") +
  xlab("Census Division") + 
  geom_hline(yintercept = 0, lty = 'dashed') + 
  coord_flip() 

# 80: --------------------------------------------------------------------------
