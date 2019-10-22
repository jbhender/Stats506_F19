## ChickWeight case study (bootstrap)
## Stats 506, Fall 2019
##
## Use the bootstrap to compute 95% confidence intervals for
## the median final weight within each diet. Do the same for the 
## median final weight relative to the initial birth weight.
## All analyses are conditional on survival to week 21.
##
## Data: datasets::Chickweight
##
## Updated: October 22, 2019
## Author: James Henderson

# libraries: ------------------------------------------------------------------
library(tidyverse)

# ChickWeight data: -----------------------------------------------------------
cw = as_tibble(ChickWeight)
str(cw, give.attr = FALSE)

# Visualize the data: ---------------------------------------------------------
cw %>% 
  filter(Time == max(Time)) %>%
  ggplot( aes( x = Diet, y = weight, fill = Diet) ) + 
  geom_boxplot( alpha = .5) + 
  theme_bw() + 
  geom_point( aes(color = Diet), position = position_jitter(width = .1) ) +
  ylab('Chick weight at 21 weeks (grams)')

# Count cases missing times: --------------------------------------------------
cw[ , .N, keyby = .(Diet, Time)][ , .( nmiss = max(N) - min(N) ), Diet]
cw %>% 
  group_by(Diet, Time) %>%
  summarize( n = n() ) %>%
  summarize( nmiss = max(n) - min(n))

# Compare starting and ending weights: ----------------------------------------
cw %>% 
  filter(Time == min(Time)) %>%
  select(Diet, Chick, start = weight) %>%
  left_join( 
    cw %>% filter(Time == max(Time) ) %>% select(Chick, end = weight),
    by = 'Chick'
  ) %>%
  ggplot( aes( x = start, y = end, color = Diet) ) +
  geom_point( alpha = .5) + 
  theme_bw()

# Compute relative weight: ----------------------------------------------------
cw = cw %>%
  group_by(Chick) %>%
  mutate( relweight = weight / weight[ Time == 0 ]) %>%
  ungroup() 

# Interactive computation the dplyr way: ---------------------------------
cw %>%
  # Ensure we pick only week 21, not earlier max time if missing data.
  ungroup() %>%
  filter( Time == max(Time) ) %>%
  group_by(Diet) %>%
  summarize( median = median(weight), n = n() )

# Progress towards functional version: ----------------------------------------
#! What if we needed to compute the median for multiple groups?
cw %>%
  ungroup() %>%
  filter( Time == max(Time) ) %>%
  group_by(Diet) %>%
  summarize_at( .vars = c("weight", "relweight"), 
                .funs = list( median = median, n = length )
  )

# Function to compute median by group for specific columns
df_group_median = function(df, cols, group ='') {
  df %>%
    group_by( .data[[!!group]] ) %>%
    summarize_at( .vars = cols, .funs = median )
}

# Test new function
df_group_median( filter(cw, Time ==  max(Time)), 
                 cols = 'weight', group = 'Diet')
df_group_median( filter(cw, Time ==  max(Time)), 
                 cols = c('weight', 'relweight'),
                 group = 'Diet')

# Function to compute a subsample: --------------------------------------------
cw_final = filter( ungroup(cw), Time == max(Time) )

# To illusrate the idea, want to retain the group sizes. 
cw_final %>%
  group_by(Diet) %>%
  mutate( bweight = weight[sample( n(), replace = TRUE )] )

df_group_boot = function(df, cols, group = ''){
  # prep list of boot functions
  boot = function(x) sample( x, replace = TRUE) 
  funs = lapply(cols, function(i) boot)
  names(funs) = paste0('b', cols)
  
  df %>%
    group_by( .data[[!!group]] ) %>%
    mutate_at( .vars = cols, 
               .funs = funs
    )
}

# Test these two functions together: ------------------------------------------ 
cw_final %>% 
  df_group_boot(., 'weight', group = 'Diet') %>%
  df_group_median(., 'bweight', group = 'Diet')

# Here's a version that does both sampling and median computation in one step.
df_boot_median = function(df, cols, group=''){

  df %>%
    group_by( .data[[!!group]] ) %>%
    mutate_at( .vars = cols, .funs = boot) %>%
#    group_by( .data[[!!group]] ) %>%
    summarize_at( .vars = cols, .funs = median )
}
# Test the above function
median_est = df_boot_median(cw_final, c('weight', 'relweight'), 'Diet')


# Create 1000 bootstrap samples.
#! How could we vectorize this?
boot_samples = list()
for(i in 1:1e3){
  boot_samples[[i]] = 
    df_boot_median(cw_final, c('weight', 'relweight'), 'Diet') %>%
    mutate( b = !! i)
}
boot_samples[[2]]
boot_samples = bind_rows(boot_samples)

boot_ci = boot_samples %>%
  group_by(Diet) %>%
  summarize_at( .vars = c('weight', 'relweight'), 
                .funs = list( lwr = function(x) quantile(x, probs = 0.025),
                              upr = function(x) quantile(x, probs = 0.975) )
  )

# Reshape to long and then to wide with bounds as columns
## bounds
boot_ci_long = 
  pivot_longer(boot_ci,
               cols = -Diet, 
               names_to = c('var', 'bound'), 
               names_sep = '_', 
               values_to = 'x') %>%
 pivot_wider(id_cols = c('Diet', 'var'), names_from = bound, values_from = x)

## point estimates
cw_median = 
  df_group_median(cw_final, c('weight', 'relweight'), group = 'Diet') %>%
  pivot_longer(cols = -Diet, names_to = 'var', values_to = 'est')

## join these two together
boot_ci_long = left_join(boot_ci_long, cw_median, by = c('Diet', 'var') )

# Plot medians and associated 95% confidence intervals for each group/var: ----
boot_ci_long %>%
  ggplot( aes(x = Diet, y = est) ) +
  geom_point( pch = 15 ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr) ) +
  facet_grid(var ~ ., scales = 'free_y' ) + 
  theme_bw() +  
  ylab('Week 21 chick weight [lower: relative to birth weight; upper: actual weight (gm)]')
