## Case Study: RECS 2009 Home types
##
## The RECS 2009 Data used in this script can be found at the link below: 
##  https://www.eia.gov/consumption/residential/data/2009/index.php?view=microdata
## In particular, see the csv data file, the code book, and the note on 
## computing standard errors using replicate weights.
##
## Author: James Henderson (jbhender@umich.edu)
## Updated: Sep 24, 2019

# 80: -------------------------------------------------------------------------

# Libraries: ------------------------------------------------------------------
library(tidyverse)

# Utility functions: ----------------------------------------------------------
source('./funcs/decode_RECS.R')

# Obtain or restore data: -----------------------------------------------------
file = './recs2009_public.RData'
if (!file.exists(file)) {
   recs_tib = readr::read_delim(
"https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public.csv",
              delim = ',' )
  save(recs_tib, file = file)
} else {
  load(file) #recs_tib
}

# replicate weights: ----------------------------------------------------------
file = './data/recs2009_weights.RData'
if ( !file.exists(file) ) {
  weights = readr::read_delim( 
  'https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public_repweights.csv',
  delim = ',')
  save(weights, file = file)
} else {
  load(file)  # weigths
}
  
# Home type by state: ---------------------------------------------------------
home_type_prop = recs_tib %>% 
  transmute(state=REPORTABLE_DOMAIN, type=TYPEHUQ, weight = NWEIGHT) %>%
  complete(state, type) %>%
  replace_na( list(weight = 0) ) %>%
  mutate(state=decode_all_states(state), type=decode_all_house_types(type)) %>%
  group_by(state, type) %>%
  summarize(homes=sum(weight)) %>%
  group_by(state) %>%
  mutate( pct = 100*homes / sum(homes) ) 
#home_type_prop

###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
home_type = recs_tib %>% 
  transmute(DOEID, state=REPORTABLE_DOMAIN, type=TYPEHUQ, weight = NWEIGHT) %>%
  replace_na( list(weight=0) ) %>%
  group_by(state, type)

# Convert weights to long: ----------------------------------------------------
weights_long = weights %>% 
  gather(key = 'repl', value = 'w', brr_weight_1:brr_weight_244 )

# Join home type to weights: --------------------------------------------------
home_type_rep = 
  weights_long %>% 
  left_join(home_type %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(home_type_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted proportions: --------------------------------------------
home_type_prop_repl = 
  home_type_rep %>%
  group_by(state, type, repl) %>%
  summarize(homes_r=sum(w)) %>%
  group_by(state, repl) %>%
  mutate( pct_r = 100*homes_r / sum(homes_r) ) 

## Missing value for IL
# with(home_type, length(unique(state)))
# with(home_type, length(unique(type)))
# nrow(home_type_prop_repl)  / 244

# Add labels and join with point estimates: -----------------------------------
home_type_prop_repl = 
  home_type_prop_repl %>% 
  ungroup() %>%
  mutate(state=decode_all_states(state), type=decode_all_house_types(type)) %>%
  left_join(home_type_prop, by = c('state', 'type') )

# Comptue standard errors: ----------------------------------------------------
home_type_prop =
  home_type_prop_repl %>%
  group_by(state, type) %>%
  summarize( pct = pct[1],
             std_err = 2 * sqrt( mean( {pct_r - pct}^2 ) )
  ) %>%
  mutate( lwr = pct - qnorm(.975)*std_err,
          upr = pct + qnorm(.975)*std_err
  )

# Reassign states to regions: -------------------------------------------------
region_map = 
  recs_tib %>% 
  group_by(Region = REGIONC, state = REPORTABLE_DOMAIN) %>%
  summarize( n = n() ) %>%
  ungroup() %>%
  mutate(Region = factor(Region, levels = 1:4, 
                         labels  = c('Northeast', 'Midwest', 'South', 'West') ),
         state = decode_all_states(state)
         ) %>%
  select( -n )

home_type_prop = home_type_prop %>%
  left_join(region_map, by = 'state')

# Plot states by region in terms of housing types: ----------------------------
p_sfd =
  home_type_prop %>% 
  ungroup() %>%
  # Order states by % single family detached
  filter( type %in% c('SingleFamilyDetached') ) %>%
  arrange( desc(pct) ) %>%
  mutate(state = factor(state, levels = state) ) %>%
  ggplot( aes( x = state, y = pct) ) +
  #geom_col( fill='gold' ) +
  geom_point( col='red', pch = 15, cex=2) + 
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) +
  facet_wrap(~Region, scales='free_x', nrow=4) +
  theme_bw() + 
  theme( axis.text.x = 
           element_text( 
             angle = 0,
             size = 10
           ) ) +
  ylim( c(0, 100) ) +
  xlab('state(s)') +
  ylab('% Single family detached homes (2009)')
p_sfd

# Function to get similar plot for any type: ----------------------------------
plot_home_type = function(type, ylab=''){
  home_type_prop %>% 
    ungroup() %>%
    # Order states by % single family detached
    filter( type %in% type ) %>%
    arrange( desc(pct) ) %>%
    mutate(state = factor(state, levels = state) ) %>%
    ggplot( aes( x = state, y = pct) ) +
    geom_point( col='red', pch = 15, cex=2) + 
    geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) +
    facet_wrap(~Region, scales='free_x', nrow=4) +
    theme_bw() + 
    theme( axis.text.x = 
             element_text( 
               angle = 0,
               size = 10
             ) ) +
    ylim( c(0, 100) ) +
    xlab('state(s)') +
    ylab( ylab )
}

#! To Do: Error bars are missing for some state:type combinations: ------------
plot_home_type('SingleFamilyAttached', '% Single family attached homes (2009)')  
plot_home_type('MobileHome', '% Mobile homes (2009)')
plot_home_type('ApartmentFew', '% Domiciles in Apartemtns with 2-4 units (2009)')
plot_home_type('ApartmentMany', '% Domiciles in Apartemtns with 5+ units (2009)')

home_type_prop %>% 
  filter( type == 'SingleFamilyAttached' & state == 'MI')

# Exercise: Modify the function above to truncate the lower bound of the 95% CI
# to zero. 
# 80: -------------------------------------------------------------------------
