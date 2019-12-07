#' ---
#' title: "Problem Set 5, Solution to Question 1"
#' author: "Stats 506, F19"
#' date: "December 7, 2019"
#' output: 
#'   html_document:
#'       theme: united
#'       code_folding: show
#' ---

#' ## About
#' This document was produced using `knitr::spin` which allows you to place
#'  markdown directly in an R script. When you write and compile Rmarkdown 
#'  (.Rmd) the `knitr::knit` function does roughly the following: 
#'  
#'   1. executes your R code and interleaves it with the markdown in a .md file
#'   1. calls pandocs to produce the outputs specified in the YAML header
#'   
#' The process is therefore: .Rmd $\to$ .md $\to$ (say) .html.
#' 
#' The knitr function `spin`` adds one level to this, allowing you to write
#' your markdown specifications directly in a .R script:
#' .R $\to$ .Rmd $\to$ .md $\to$ .html. 
#' I find this most useful for documents where the amount of text and 
#' markdown formatting is smaller or on par with the amount of actual R code.
#' 
#' From within Rstudio, you can compile the source (.R) script using 
#' `cmd + shift + k` or whatever your compile shortcut is.
#'  
#' In a spin document, markdown goes after a special comment `#'`.  R code
#' chunks and associated options follow `#+` (among other options).
#' 
#' The source code for this document can be found at the course
#' [repo](https://github.com/jbhender/Stats506_F19/tree/master/problem_sets) as
#' `ps5_q1_spin.R`. See also `ps5_q1.R` for the R script without markdown.
#' 
#' ## Question
#' Our goal in this problem is to:
#' 
#' > Compare disparities in
#' internet access betwen urban (including "urban cluster") and rural areas.
#' 
#' ## Header 
#' 
#+ header
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

#' ## R packages
#+ libraries, messages = FALSE, warnings = FALSE
# libraries: -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse); library(data.table)
})

#' ## Data import
#+ data
# data: ------------------------------------------------------------------------
recs = fread('./recs2015_public_v4.csv')

#' ## Point estimates
#+ point estimates
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

#' ## Standard Errors
#' To compute the standard errors, we first pivot the weights to a long format.
#+ long_weights
# pivot replicate weights to a long form for standard error computations: ------
recs_weights = melt(recs, 
                    id.vars = 'DOEID', 
                    measure.vars = grep('^BRR', names(recs), value = TRUE),
                    value.name = 'w',
                    variable.name = 'repl'
)
#' Next, we recompute the point estimates for each group for each set of 
#' replicate weights.

#+ replicate_estimates
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

#' Now we can compute the standard errors by scaling the square root of the 
#' mean squared deviations between the replicate and point estimates.

#+ se_comps
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

#' After computing the standard errors, we can form 95% confidence bounds. 
#' Here we do so using data.table reference semantics.

#+ cis
# construct CI bounds and drop se using reference semantics: -------------------
m = qnorm(.975)

prop_internet_ur[ , `:=`(lwr = prop_internet - m * se,
                         upr = prop_internet + m * se,
                         se = NULL)]

disp_internet_ur[ , `:=`(lwr = delta_internet - m * se,
                         upr = delta_internet + m * se,
                         se = NULL)]

#' ## Results
#' In this section we present our results. First, we order the divisions
#' by the level of disparity in internet access and add the divison names.
#' In the code below, we are implicitly ordering using `data.table::setkey()`.
#' Like all `set()` functions in data.table, this orders by reference -- the
#' data is physically reordered in place. 

#+ order_divisions
# make plots/tables more interpretable by ordering by disparity: ---------------

## order
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

## apply order
prop_internet_ur[ , division := factor(DIVISION, div_ord, 
                                       labels = divisions[div_ord])]
setkey(prop_internet_ur, division)

disp_internet_ur[ , division := factor(DIVISION, div_ord, 
                                       labels = divisions[div_ord])]
setkey(disp_internet_ur, division)

#' Next we format our results as a table. In this example, we use the html
#' tags `<center>` and `<br>` (line break) to improve the appearance of the results.

#+ table
# Make a table with all three pieces of information: --------------------------
tab1 =
  prop_internet_ur[ , .(division, urban,
                      ci = sprintf('<center>%4.1f <br>(%4.1f, %4.1f)</center>', 
                                   100 * prop_internet, 100 * lwr, 100 * upr) )
 ] %>%
 dcast(., division ~ urban, value.var = 'ci' )

tab1 = 
  disp_internet_ur[ , 
   .(division, disparity = sprintf('<center>%4.1f <br>(%4.1f, %4.1f)</center>', 
                             100 * delta_internet, 100 * lwr, 100 * upr) ) 
   ] %>%
   merge( tab1, ., by = 'division') 

tab1 = tab1[order(-division)]
setnames(tab1, c('Census Division', 
                 'Rural, % <br> (95% CI)',
                 'Urban, % <br> (95% CI)',
                 'Disparity, % <br> (95% CI)'
                 ) 
         )
DT::datatable(tab1, escape = FALSE)

#' Finally, we plot the estiamtes proportions and disparities. 

#+ plot_props, fig.cap = cap1
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
cap1 = paste(
  '**Figure 1.** *Estimated proportion of homes with internet access by',
  'rurality in each Census Division.*')
p1

#+ plot_disp, fig.cap = cap2
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
cap2 = paste(
  '**Figure 2.** *Estimated disparity in internet access (urban less rural) by',
  'Census Division.*'
)
p2





