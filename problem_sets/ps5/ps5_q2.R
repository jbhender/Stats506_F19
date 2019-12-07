# Analysis of Methylation in Adipocyte Tissues for Crohn's Disease
# Stats 506, Fall 2019
#
# Data from 
# ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE138nnn/GSE138311/matrix/
#
# Details: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE138311
#
# Author: James Henderson
# Updated: December 4, 2019
# 80: --------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(tidyverse); library(data.table); library(future)

# (b) read in the data: --------------------------------------------------------
#setwd('~/github/ps506/F19/ps5/')
df = fread('gunzip -cd ./GSE138311_series_matrix.txt.gz', skip = 68)

# (b) subset to chromosomonal sites: -------------------------------------------
df_chrom = df[grep('^ch', ID_REF)]

# (b) remove missing sample: ---------------------------------------------------
df_chrom[ , GSM4105199 := NULL]

# (b) transform to a long format: ---------------------------------------------
chrom_long = 
  melt( df_chrom, id.vars = 'ID_REF',
        variable.name = 'sample', value.name = 'methyl')

# (c) crohn's and no-crohn's samples: -----------------------------------------
crohn_names = paste0("GSM41051", 87:93)
norm_names = paste0("GSM41051", 94:98)

chrom_long[ , `:=`(sample_group =
                     ifelse( sample %in% crohn_names, "crohn", "normal"))]


# (d) compute a t-statistic for each methylation site: ------------------------
## assuming homogenous variance
chrom_t = chrom_long[ , .(m = mean(methyl), v = var(methyl), N = .N) , 
            keyby = .(ID_REF, sample_group)]

## keyby, above, ensures sample_groups are always in the same order
chrom_t = 
  chrom_t[ , 
    .(tstat = diff(m) / sqrt( sum( {N - 1} * v ) / {sum(N) - 2} * sum(1 / N))), 
    by = .(ID_REF)]

# (e) add a column, "probe_group" by reference based on first 5 digits of ID
chrom_t[ , `:=`(probe_group = str_sub(ID_REF, 1, 5))]


# (f) compute proportion of nominally significant sites by group and plot: -----
## Note chromsome 14 has a high percentage of nominally significant 
##  t-stats with magnitude greater than 2.22
prop_nom_sig = 
  chrom_t[ , .(p = mean({abs(tstat) > qt(.975, df = 10)} * abs(tstat))), 
           probe_group]

prop_nom_sig = prop_nom_sig[order(p)]
prop_nom_sig[ , probe_group := factor(probe_group, probe_group)]
ggplot(prop_nom_sig, aes( y = p, x = probe_group) ) +
  geom_col() +
  theme_bw() + 
  xlab("Chromosome") +
  ylab("proportion of methylation sites nominally significant") +
  ggtitle("Comparing adipocyte methylation in Crohn's to Non-Crohn's.") +
  ylim(c(0, .5)) +
  geom_hline( yintercept = 0.05, color = 'darkred')

# (g) function for computimg group-level stat on original or permtued data: ----
compute_custom_stat = function(chrom_long, 
                               permute = FALSE,
                               type = c('two-tail', 'greater', 'lesser')
                      ) {

    # match type requested
    type = match.arg(type, c('two-tail', 'greater', 'lesser'))
    stopifnot( type %in% c('two-tail', 'greater', 'lesser') )
    
    # If a permuation is asked for, copy the data and permute the group label
    if ( permute ) {
      perm_groups = chrom_long[ , .N, .(sample, sample_group)]
      perm_groups[ , sample_group := sample(sample_group, replace = FALSE)]
      
      chrom_long = merge( chrom_long[, !"sample_group"], perm_groups, 
                           by = 'sample')
    }
    # Compute means and variance by group
    chrom_t = chrom_long[ , .(m = mean(methyl), v = var(methyl), N = .N) , 
                        keyby = .(ID_REF, sample_group)]
    df = chrom_t[, .(N = unique(N)), sample_group][, sum(N) - .N]
    ## Compute pooled t-statistic for each gene
    chrom_t = 
      chrom_t[, .(tstat = diff(m) / 
                    sqrt( sum( {N - 1} * v ) / {sum(N) - 2} * sum(1 / N))
                  ), 
                 by = .(ID_REF)]
    chrom_t[ , `:=`(probe_group = str_sub(ID_REF, 1, 5))]
    
    ## Find proportion of t-stats with absolute value greater than 2, and
    ##  scale by size of t-stat
    if ( type == 'two-tail' ) {
      return( chrom_t[, mean( {abs(tstat) > qt(.975, df = df)} * abs(tstat) ),
                      probe_group] 
      )
    } else if ( type == 'greater' ) {
      return( 
        chrom_t[, mean( {tstat > qt(.95, df = df)} * tstat), probe_group] 
      )
    } else {
      return(
        chrom_t[, mean( {-tstat > qt(.95, df = df)} * tstat), probe_group] 
      )
    }
}
#compute_custom_stat(chrom_long, permute = FALSE, type = 'greater')
#compute_custom_stat(chrom_long, permute = FALSE, type = 'lesser')
#compute_custom_stat(chrom_long, permute = FALSE, type = 'two-tail')

# (h) permute for T_abs, sequentially: -----------------------------------------
##Serial time for 1000 permutations, using two threads for data.table operations 
nperm = 1e3
system.time({
  perm_list = vector(mode = 'list', length = nperm)
  for( perm in 1:nperm) {
    perm_list[[perm]] = compute_custom_stat(chrom_long, permute = TRUE)
    if( perm %% 100 == 0) cat(perm, "perms\n")
  }
})

# Serial time for 1000 permutations using one thread for data.table operations
setDTthreads(threads = 1)
system.time({
  perm_list = vector(mode = 'list', length = nperm)
  for( perm in 1:nperm) {
    perm_list[[perm]] = compute_custom_stat(chrom_long, permute = TRUE)
    if( perm %% 100 == 0) cat(perm, "perms\n")
  }
})

# (i) permute for T_up, using mclapply: ----------------------------------------
# Note that dtThreads is still set to 1.
system.time({
  perm_list_up = 
    parallel::mclapply(1:nperm, function(i) {
      compute_custom_stat(chrom_long, permute = TRUE, type = 'greater')
    }  )
})

# (j) permute for T_down, using futures: ---------------------------------------
# Note that dtThreads is still set to 1.
nworkers = 2
plan(multisession, workers = nworkers)

## Based on sequential timing, we know each function call takes ~0.04 seconds
##  we want to employ prescheduling manually to make this more feasible. 
##  Compare the result to issuing `nperm`` futures in a simple for loop,
##  which would be like setting mc.preschedule = FALSE.
system.time({
  future_list = vector( mode = 'list', length = nworkers)
  for( i in 1:nworkers ) {
    
    # This code chunk splits the perms into chunks, and handles cases where
    # nperm is not an exact multiple of nworkers
    if ( i == nworkers ){
      chunk_perms = nperm - {nworkers - 1}*nperm %/% nworkers
    } else {
      chunk_perms = nperm %/% nworkers
    }
    
    future_list[[i]] = future({
      lapply( 1:chunk_perms, 
       function(n) {
         compute_custom_stat(chrom_long, permute = TRUE, type = 'greater')
        }
      )
    }) # ends the future
  } # ends the for loop

  # result above is a nested list of futures  
  perm_list_down = lapply( do.call("c", future_list), value ) 
})

# A quick analysis of each set of tests: ---------------------------------------
## You weren't asked to do this, but you may want to see the results.

## two-tailed
chrom_perm_abs = rbindlist(perm_list)
chrom_obs = compute_custom_stat(chrom_long, permute = FALSE)
chrom_perm_abs = 
  merge(chrom_perm_abs, 
        chrom_obs[ , .(probe_group, obs = V1)],
        by = 'probe_group',
        all.x = TRUE
  )
chrom_perm_abs[ , .( p = {1 + sum(V1 >= obs)} / .N), probe_group][order(-p)]


