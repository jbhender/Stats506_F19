# Stats 506, Fall 2019
# Problem Set 2, Question 2
#
# This script analyzes the mouse tracking data from KH2017
#
# Author: James Henderson
# Date: October 14, 2019
#80: ---------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse); library(lme4); library(lmerTest)

# data: -----------------------------------------------------------------------
df = as_tibble( mousetrap::KH2017_raw )

# utility functions: ----------------------------------------------------------

## (a) functions that compute measures
source('./ps2_q2_funcs.R')

## (b) function to put the data into a useable form
make_numeric = function(x){
  # Inputs: A character vector string of x or y positions of the form 
  #         "[x1, x2, ..., xt]"
  # Outputs: a list of numeric vectors, e.g. c(x1, ..., xt)
  
  # Convert to character if needed
  if ( is.factor(x) ) {
    x = as.character(x)
    warning("make_numeric: converting x to character")
  }
  
  # Make replacements and conversions
  lapply(str_split(str_replace_all(x,'\\[|\\]', ''), ", "), as.numeric)
}

# (b) extract trajectories to numeric "list" columns: --------------------------
df = df %>%
  mutate( 
      xpos = make_numeric( as.character(xpos_get_response) ),
      ypos = make_numeric( as.character(ypos_get_response) ),
      tm   = make_numeric( as.character(timestamps_get_response) )
  )

# (c) compute the curvature measures for trial and attach to original data: ---
df_measures = df %>%
  filter( correct == 1 ) %>%
  group_by(subject_nr, count_trial) %>%
  do( 
   convert_vec2tibble( 
    comp_measures( 
      normalize_traj( cbind(.$xpos[[1]], .$ypos[[1]], .$tm[[1]] ) ) 
    )
   ) 
  )

## join measures 
df = left_join(df_measures, df, by = c('subject_nr', 'count_trial'))

# (d) use an lmm to compare the impact of "condition" on each curvature measure

## Make "typical" the reference level 
df = mutate(df, Condition = factor(Condition, c('Typical', 'Atypical')))

## Compute differences by Condition for each curvature measure.
fit_tot_dist = 
  lmer( log( tot_dist ) ~ Condition + (1 | subject_nr) + (1 | Exemplar),
        data = df)

fit_max_abs_dev = 
  lmer( log( max_abs_dev ) ~ Condition + (1 | subject_nr) + (1 | Exemplar),
        data = df)

fit_avg_abs_dev = 
  lmer( log( avg_abs_dev ) ~ Condition + (1 | subject_nr) + (1 | Exemplar),
        data = df)

# For AUC < 1, we will impute 1.
fit_AUC = 
  lmer( log( pmax(AUC, 1) ) ~ Condition + (1 | subject_nr) + (1 | Exemplar),
        data = df)

## Function to compute a simple "Wald" type confidence interval
wald_ci = function(fit, term = 'ConditionAtypical',  level = .95,
                   f = function(x) exp(x) ) {
   se = summary(fit)$coefficients[term, "Std. Error"]
   b = fixef(fit)[term]
   m = qnorm( 1 - {1 - level}/2 )
   
   data.frame( est = f(b), lwr = f(b - m * se), upr = f(b + m * se) )
}
#wald_ci(fit_AUC)

## Function to extract confidence intervals for the variance of the random
## effects using proflie likelihood. (You did not need to do this for the
# assignment.)
prof_ci = function(fit, comps = c('Subject', 'Exemplar', 'Error')) {
  # compute CIs for variance components using profile likelihood
  # don't, but could, also use this profile likelihood for CIs of fixed effects
  # Inputs: 
  #   fit - a merMod fit, as from lmer
  #   comps - a character vector giving names to the variance components
  # Outputs: a tibble with columns "Component" (using input 'comp') as well as:
  #          "est", "lwr", and "upr" with the estimated variance and CI limits.
  prof = confint(fit)
  keep = grep(".sig", rownames(prof))
  as_tibble(prof[keep, ]) %>%
    transmute( 
      Component = comps,
      est = c(unlist( lapply(VarCorr(fit), attr, "stddev")), sigma(fit) ), 
      lwr = `2.5 %`,
      upr = `97.5 %`
    )
}
#prof_ci(fit_AUC)

# Use the first function on each fit, then format as data frame/tibble
ci_list = 
  lapply(list(fit_tot_dist, fit_max_abs_dev, fit_avg_abs_dev, fit_AUC), wald_ci)
ci_df = bind_rows(ci_list) %>% 
  mutate(measure = c('Total Distance', 'Maximum Absolute Deviation',
                     'Average Absolute Deviation', 'AUC')
         ) %>%
  mutate( `Relative Effect` = sprintf('%4.2f (%4.2f-%4.2f)', est, lwr, upr))

# Use the second function on each fit for the SDs of each variance component.
ci_varcomp = 
  lapply(list(fit_tot_dist, fit_max_abs_dev, fit_avg_abs_dev, fit_AUC), prof_ci)

ci_df = left_join(ci_df,
  bind_rows(ci_varcomp) %>%
  mutate(measure = rep(c('Total Distance', 'Maximum Absolute Deviation',
                     'Average Absolute Deviation', 'AUC'), each = 3)
  ) %>%
  mutate(SD = sprintf('%4.2f (%4.2f-%4.2f)', est, lwr, upr)) %>%
  pivot_wider(id_cols = measure, names_from = Component, values_from = SD),
  by = 'measure' 
)

# plot to compare
#! This figure is also reproduced within the markdwon document
ci_df %>%
  arrange( est ) %>%
  mutate( measure = factor(measure, levels = unique(measure) ) ) %>%
  ggplot( aes( y = measure, x = est) ) +
  geom_point() +
  geom_errorbarh( aes(xmin = lwr, xmax = upr) ) + 
  geom_vline( xintercept = 1, lty = 'dashed') + 
  xlab('relative effect on curvature measure of an atypical exemplar') + 
  ylab('') +
  theme_bw()
