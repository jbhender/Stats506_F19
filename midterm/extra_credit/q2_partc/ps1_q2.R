## Problem Set 1, Question 2
## Stats 506, F18
##
## Answers questions about flights originating from the NYC area in 2013-2014.
## Uses 2013 data from the nycflights13 R package
## and 2014 data from the link below:
# 'https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv
##
## Author: James Henderson
## Updated: September 29, 2018

# libraries: ------------------------------------------------------------------
# install.packages('nycflights13')
library(tidyverse)
library(lubridate)
library(nycflights13)

# See the tables included in nycflights13
# pos = match('package:nycflights13', search())
# ls( pos = as.environment(pos) )

# 2014 data: ------------------------------------------------------------------
file = './nycflights14.csv'
if ( !file.exists(file) ){
  flights14 = 
    readr::read_delim('https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv',
                      delim = ',')
  readr::write_delim(nycflights14, path = file, delim=',')
} else {
   flights14 = readr::read_delim(file, delim=',')
}

## Construct the date of the flight: ------------------------------------------
flights = flights %>% 
  mutate( date = ymd( sprintf('%i-%02i-%02i', year, month, day ) ) )

flights14 = flights14 %>% 
  mutate( date = ymd( sprintf('%i-%02i-%02i', year, month, day ) ) )
  
# a, arlines representing 1% of flights from 2013 (through October): ----------
top_carriers = flights %>% 
  filter( date <= ymd('2013-10-31') ) %>%
  group_by(carrier) %>%
  summarize(n = n()) %>%
  mutate( p = 100 * n / sum(n) ) %>%
  arrange( desc(p) ) %>%
  filter( p > 1) %>%
  left_join(airlines, by = 'carrier') %>%
  select(Airline = name, carrier, n, p )

# Get carrier counts for 2013: -----------------------------------------------
tot_2013 = 
  flights %>%
  filter( ymd( paste(year, month, day, sep ='-') ) < ymd('2013-11-01') ) %>%
  group_by(carrier) %>%
  summarize(n = n())  %>%
  arrange(-n) %>%
  mutate( phat = n/sum(n),
          se = sqrt(phat*{1-phat}/n),
          lwr = phat - qnorm(.975)*se,
          upr = phat + qnorm(.975)*se,
          prop = sprintf('%3.1f%% (%3.1f, %3.1f)',
                         100*phat, 100*lwr, 100*upr),
          n_nice = format(n, big.mark = ',')
  ) %>%
  filter(phat > .01) %>%
  left_join(airlines, by = 'carrier') %>%
  select(
    # Pretty vars
    name, n_2013 = n_nice, p_2013 = prop,
    # Working vars
    n13 = n, p13 = phat, se13 = se
  )

# Flight totals for 2014: -----------------------------------------------------
tot_2014 = 
  flights14 %>%
  filter( ymd( paste(year, month, day, sep ='-') ) < ymd('2014-11-01') ) %>%
  group_by(carrier) %>%
  summarize(n = n())  %>%
  arrange(-n) %>%
  mutate( phat = n/sum(n),
          se = sqrt(phat*{1-phat}/n),
          lwr = phat - qnorm(.975)*se,
          upr = phat + qnorm(.975)*se,
          prop = sprintf('%3.1f%% (%3.1f, %3.1f)',
                         100*phat, 100*lwr, 100*upr),
          n_nice = format( n, big.mark = ',')
  ) %>%
  #  filter(phat > .01) %>%
  left_join(airlines, by = 'carrier') %>%
  select(
    # Pretty versions
    name, n_2014 = n_nice, p_2014 = prop, 
    # Working versions
    n14 = n, p14 = phat, se14 = se
  )

# Join and compute changes: --------------------------------------------------
top_carriers_13v14 = tot_2013 %>%
  left_join(tot_2014, by = 'name') %>%
  # Replace values for any airlines not found in 2014
  replace_na( list(n_2014 = '-', p_2014 = '-',
                   n14 = 0, p14 = 0, se14 = 0 ) 
  ) %>%
  # Compute differnces
  mutate( delta = p14 - p13,
          delta_se = sqrt(se13^2 + se14^2),
          lwr = delta - qnorm(.975)*delta_se,
          upr = delta + qnorm(.975)*delta_se,
          d = sprintf( '%4.1f%% (%4.1f, %4.1f) ', 100*delta, 100*lwr, 100*upr )
    )

# Test Delta vs Jet Blue increase (not required): ----------------------------
# Approximate z-test for whether Delta airlines increase is 
# significantly larger than the Jet Blue increase
z =
  with(
  top_carriers_13v14 %>%
  filter( grepl('^Delta', name) |
          grepl('^Jet', name)
  ),
  diff(delta) / sqrt( sum(delta_se^2) )
)
p = 2*{1 - pnorm(z)}

# Part c, total flights by carrier and aiport: --------------------------------
## Note teh use of `fill = '-'` to replace missing values in the call to spread.
carrier_origin =
  bind_rows( flights %>% select(origin, carrier),
             flights14 %>% select(origin, carrier) 
  ) %>%
  group_by(origin, carrier) %>%
  summarize(n = n())  %>%
  group_by(origin) %>% ## Not needed but clearer
  mutate( phat = n/sum(n),
          se = sqrt(phat*{1-phat}/n),
          lwr = phat - qnorm(.975)*se,
          upr = phat + qnorm(.975)*se,
          prop = sprintf('%3.1f%% (%3.1f, %3.1f)', 100*phat, 100*lwr, 100*upr)
  ) %>% 
  select(carrier, origin, prop) %>%
  spread(origin, prop, fill = '-') %>%
  left_join(airlines, by = 'carrier') %>%
  filter(name %in% {tot_2013 %>% filter(p13 > .01)}$name)

# This is a trick to ensure the final table is sorted: ------------------------
carrier_origin = carrier_origin %>%
  mutate( name = factor(name, levels = top_carriers$Airline) ) %>%
  arrange( name )
