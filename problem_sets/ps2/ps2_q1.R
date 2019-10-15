# Stats 506, Fall 2019
# Problem Set 2, Question 1
#
# In this script, I use the 2015 RECS data (link below) to examine home 
# heating behavior in the US.
#
# Data Source:
# https://www.eia.gov/consumption/residential/data/2015/index.php?view=microdata
#
# Updated: October 14, 2019
# Author: James Henderson

# libraries: -------------------------------------------------------------------
library(tidyverse)

# data: ------------------------------------------------------------------------
#https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
file = './recs2015_public_v4.csv'
recs = read_delim(file, delim = ',')

# codebook: --------------------------------------------------------------------
#https://www.eia.gov/consumption/residential/data/2015/xls/codebook_publicv4.xlsx
path = './'
codebook_file = sprintf('%s/codebook_publicv4.xlsx', path)
codebook = readxl::read_excel(codebook_file, skip = 3) %>%
  as.data.frame()
codes = 
  codebook %>% 
  transmute(
   variable = `SAS Variable Name`,
   desc = `Variable Description`,
   levels = stringr::str_split(`...5`, pattern = '\\r\\n'),
   labels = stringr::str_split(`Final Response Set`, pattern = '\\r\\n')
  ) %>%
  # to suppress warnings in the function below
  as.data.frame()

decode_recs = function(x, var, codebook = codes ) {
  # transform a recs variable into a factor with labels as given in 
  # the codebook under "Final Response Set".
  # Inputs:
  #  x - a vector of factor levels to be decoded, e.g. a column in recs data
  #  var - a length 1 character vector with names to be decoded
  #  codes - the codebook in which factor levels & labels are found as columns
  #           with those names and a row for which column "variable" matches 
  #           xname
  # Returns: x, transformed to a factor
  
  #if ( xname %in% codes$variable ) {
  #  cat('ok\n')
    #labels = codes[ codes$variable == xname, ]$labels[[1]]
    #levels = codes[ codes$variable == xname, ]$levels[[1]]
    #factor(x, levels = levels, labels = labels)
  #} 
  if ( var %in% codebook$variable ) {
    labels = codebook[ codebook$variable == var, ]$labels[[1]]
    levels = codebook[ codebook$variable == var, ]$levels[[1]]
    factor(x, levels = levels, labels = labels)
  } else {
    msg = sprintf('There is no variable "%s" in the supplied codes.\n', var)
    stop(msg)
  }
}
#decode_recs(c('U', 'R', 'C'), "UATYP10")

# clean up key variables used in this problem: ---------------------------------
recs_core = 
  recs %>% 
  transmute( 
    id = DOEID,
    weight = NWEIGHT, 
    division = decode_recs(DIVISION, 'DIVISION'),
    urban = decode_recs(UATYP10, 'UATYP10'),
    heat_home = decode_recs(HEATHOME, 'HEATHOME'),
    fuel = decode_recs(FUELHEAT, 'FUELHEAT'),
    heat_behavior = EQUIPMUSE,#decode_recs(EQUIPMUSE, 'EQUIPMUSE'),
    temp_home = TEMPHOME,
    temp_gone = TEMPGONE,
    temp_night = TEMPNITE
  ) %>%
  # Convert negative numbers to missing, for temps. 
  mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))
  #! You could also type these conversions directly ...
  # transmute( 
          # urban = factor(urban, levels = c('U', 'C', 'R'), 
          #               labels = c('Urban', 'Urban Cluster', 'Rural')),
  #)

#recs_core %>% summarize_all( .funs = function(x) sum(x < 0, na.rm = TRUE))
#recs_core %>% group_by(heat_home) %>% summarize( n = n(), nna = sum( is.na(temp_night) ) )
#recs_core %>% group_by(heat_home) %>% summarize( n = n(), nna = sum( is.na(heat_behavior) ) )

# replicate weights, for computing standard errors: ----------------------------
weights_long = 
  recs %>% 
  select( id = DOEID, BRRWT1:BRRWT96 ) %>%
  pivot_longer( cols = BRRWT1:BRRWT96, 
                names_to = 'replicate', 
                values_to = 'weight'
  )

# (a) national average nightime temperature in winter: ------------------------

## point estimate
temp_night = 
  recs_core %>% 
  filter( heat_home == 'Yes' & !is.na(temp_night) ) %>%
  summarize( temp_night = sum(temp_night * weight) / sum(weight))

## replicate estimates
temp_night_rep = 
  recs_core  %>% 
  filter( heat_home == 'Yes' & !is.na(temp_night) )%>%
  select(id, temp_night) %>%
  left_join( weights_long, by = c('id') ) %>%
  group_by(replicate) %>%
  summarize(  temp_night = sum(temp_night * weight) / sum(weight) )

## standard error
### below, !! is used to evaluate temp_night in the global environment
###  rather than referring to the column within temp_night_rep
temp_night = 
  temp_night_rep %>%
  mutate( estimate = !!temp_night$temp_night ) %>% 
  summarize( pe = estimate[1], 
             se = 2 * sqrt( mean( {temp_night - estimate}^2 ) ) ) %>%
  mutate( lwr = pe - qnorm(.975) * se, upr = pe + qnorm(.975) * se )

## string with answer
str_temp_night =
  with(temp_night, sprintf('%4.1f (95%% CI, %4.1f-%4.1f)', pe, lwr, upr) )

# (b) fuel type used for space heating, by division and rural status: ---------

## point estimates
fuel_types = 
  recs_core %>% 
  filter( heat_home == "Yes" ) %>%
  # dropping "Not Applicable" b/c it disappears with the filter above
  mutate(fuel = factor(fuel, levels = levels(fuel)[1:6])) %>%
  group_by(division, urban, fuel) %>%
  summarize( n = sum(weight), nr = n() ) %>%
  # the above summarize removes grouping by fuel
  mutate( N = sum(n), pct = 100 * n / N ) %>%
  #select(division, urban, fuel, pct) %>% 
  # provide explicit missing values, but not by group
  ungroup() %>%
  complete(division, urban, fuel, fill = list(pct = 0) ) 

## replicate estimates
fuel_types_rep = recs_core %>% 
  filter( heat_home == "Yes" ) %>%
  # dropping "Not Applicable" b/c it disappears with the filter above
  mutate(fuel = factor(fuel, levels = levels(fuel)[1:6])) %>%
  select(id, division, urban, fuel) %>%
  left_join(weights_long, by = 'id') %>%
  group_by(division, urban, replicate, fuel) %>%
  summarize( n = sum(weight) ) %>%
  # the above summarize removes grouping by fuel
  mutate( N = sum(n), pct = 100 * n / N ) %>%
  select(division, urban, fuel, replicate, rep_pct = pct)

## standard errors and 95% CI's
fuel_types_ci =
  left_join(fuel_types_rep, fuel_types, 
            by = c('division', 'urban', 'fuel') 
  ) %>%
  group_by(division, urban, fuel) %>%
  summarize( 
    est = pct[1],
    se = 2 * sqrt( mean( {rep_pct - pct}^2 ) )         
 ) %>%
 mutate(
   lwr = pmax(est - qnorm(.975) * se, 0),
   upr = pmin(est + qnorm(.975) * se, 100),
   pct_fuel = sprintf('%02.0f (%02.0f-%02.0f)', est, lwr, upr)
 )  

# provide explicit missing values, but not by group
fuel_types_ci = 
  fuel_types_ci %>%
  ungroup() %>%
  complete(division, urban, fuel, fill = list(pct_fuel = '-') ) 

# make wide for table
fuel_types_wide =
  fuel_types_ci %>%
  pivot_wider( id_cols = c('division', 'urban'),
               names_from = 'fuel', 
               values_from = 'pct_fuel')

# (c) compare home, gone, and night winter temps, by division and rurality: ---

## lengthen data to do all temps at once
temps_long =
  recs_core %>%
  pivot_longer( cols = starts_with("temp"),
                values_to = "temp",
                names_to = "type", 
                names_prefix = "temp_"
                
  ) %>%
  select( id, weight, division, urban, type, temp)

## point estimates
temps_pe =
  temps_long %>%
  filter( !is.na(temp) ) %>%
  group_by(division, urban, type) %>%
  summarize( pe = sum(temp * weight) / sum(weight) )

## replicate estimates
temps_rep = 
  left_join(temps_long %>% select(-weight), weights_long, by = 'id') %>%
  filter( !is.na(temp) ) %>%
  group_by(division, urban, replicate, type) %>%
  summarize( temp = sum(temp * weight) / sum(weight) )

## standard errors 
temps = 
  left_join(temps_rep, temps_pe, by = c('division', 'urban', 'type')) %>%
  group_by(division, urban, type) %>%
  summarize( est = pe[1], se = 2 * sqrt( mean( {temp - pe}^2) ) ) %>%
  mutate( lwr = est - qnorm(.975) * se, upr = est + qnorm(.975) * se)
  
## plot these values
### order divisions by average "home" temp 
div_order = with(
  temps %>%
  filter( type == 'home' ) %>%
  group_by( division ) %>%
  summarize( est = mean(est) ) %>%
  arrange(est),
  as.character( division) )

#! Note, plot is reconstructed in the markdown document.
temps_plot = 
  temps %>%
  ungroup() %>%
  mutate( division = factor(division, levels = div_order),
          `Temperature Type` = type
  ) %>%
  ggplot( aes(y = est, x = division, color = `Temperature Type`, 
              group = type) ) +
  geom_point( position = position_dodge(width = .5), alpha = 0.5 ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr), alpha = 0.5, 
                 position = position_dodge(width = .5)
  ) + 
  coord_flip() +
  facet_wrap(~urban) + 
  theme_bw() +
  scale_color_manual( values = c('goldenrod', 'darkorange', 'black')) +
  ylab( expression('Temperature ' (degree*'F')) ) +
  ylim( c(60, 75)) +
  xlab('')
    

# (d) National median difference between night and day temps, for
#     each type of thermostat behavior: ----------------------------------------

## point estimate
temp_diff_pe =
  recs_core %>% 
  filter( heat_home == 'Yes' ) %>%
  mutate( temp_diff = temp_home - temp_night ) %>%
  select( heat_behavior, weight, temp_diff) %>%
  # computes the weighted median
  arrange( heat_behavior, temp_diff ) %>%
  group_by( heat_behavior ) %>%
  mutate( cumsum_weight = cumsum(weight), total_weight = sum(weight) ) %>%
  filter( cumsum_weight >= .5 * total_weight) %>%
  summarize( temp_diff = min(temp_diff) )
  
## replicate estimates
temp_diff_reps =
  left_join( weights_long, 
             recs_core %>% 
               mutate( temp_diff = temp_home - temp_night ) %>%
               select( id, heat_home, heat_behavior, temp_diff),
             by = 'id'
  ) %>% 
  filter( heat_home == 'Yes' ) %>%
  # computes the weighted median
  arrange(heat_behavior, replicate, temp_diff) %>%
  group_by(heat_behavior, replicate) %>%
  mutate(cumsum_weight = cumsum(weight), total_weight = sum(weight)) %>%
  filter( cumsum_weight >= .5 * total_weight ) %>%
  summarize( temp_diff_rep = min(temp_diff) )

## standard errors
temp_se = 
  left_join(temp_diff_reps, temp_diff_pe, by = 'heat_behavior') %>%
  group_by(heat_behavior) %>%
  summarize(
    est = temp_diff[1],
    se = 2 * sqrt( mean( {temp_diff_rep - temp_diff}^2 ) )
  ) %>%
  mutate(
    lwr = est - qnorm(.975) * se,
    upr = est + qnorm(.975) * se,
    temp_diff = sprintf('%1.0f (%3.1f-%3.1f)', est, lwr, upr)
  )

## plot
hb_order = with( temp_se %>% arrange(est, lwr), heat_behavior)
hb_labels = c('Set temp', 'Manually adjust', 
  'Program thermostat', 'Turn on/off', 
  'No control', 'X', 'X', 'X', 'Other')[hb_order]

#! Note, plot is reconstructed in the markdown document.
plot_med_diff = 
  temp_se %>%
  mutate( heat_type = factor( heat_behavior, hb_order, hb_labels) ) %>%
  ggplot( aes( y = heat_type, x = est) ) +
  geom_point() +
  geom_errorbarh( aes(xmin = lwr, xmax = upr)) +
  theme_bw() +
  ylab("Heating Equipment Behavior") + 
  xlab( 
    expression("Median difference between day and night temperatures "
               (degree*'F'))
  )
