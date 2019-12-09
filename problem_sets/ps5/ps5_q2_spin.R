#' ---
#' title: "Problem Set 5, Solution to Question 2"
#' author: "Stats 506, F19"
#' date: "December 7, 2019"
#' output: 
#'   html_document:
#'       theme: united
#'       code_folding: show
#' ---

#' ## Header
#+ header
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

#' ## Libraries
# libraries: -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse); library(data.table); library(future)
})

#' ## a
#' To scroll the first few lines of the compressed data use:
#' ```bash
#' < GSE138311_series_matrix.txt.gz gunzip -cd | less -S
#' ```
#' You could find the line number for the header row using:
#' ```bash
#' < GSE138311_series_matrix.txt.gz gunzip -cd | grep -n "ID_REF"
#' ```
#' There are 68 rows of metadata, with column headers beginning on line 69.
#' There is also a tag at the end of the file, we will let `fread` handle this
#' here.
#' 

#' ## b
#' In this part we read and format the data, pivoting to a longer format with
#' one row per methylation site.

#+ b
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

#' ## c
#' In this part we associate each sampel with the appropriate Crohn's or not
#' Crohn's group.

#+ c
# (c) crohn's and no-crohn's samples: -----------------------------------------
crohn_names = paste0("GSM41051", 87:93)
norm_names = paste0("GSM41051", 94:98)

chrom_long[ , `:=`(sample_group =
                     ifelse( sample %in% crohn_names, "crohn", "normal"))]

#' ## d
#' Next, we compute t-statistics for each methylation site / probe. 

#+ d
# (d) compute a t-statistic for each methylation site: ------------------------
## assuming homogenous variance
chrom_t = chrom_long[ , .(m = mean(methyl), v = var(methyl), N = .N) , 
            keyby = .(ID_REF, sample_group)]

## keyby, above, ensures sample_groups are always in the same order
chrom_t = 
  chrom_t[ , 
    .(tstat = diff(m) / sqrt( sum( {N - 1} * v ) / {sum(N) - 2} * sum(1 / N))), 
    by = .(ID_REF)]

#' ## e
#' Next, we associate methylation sites / probes with the chromosomes where 
#' they appear. Note the use of reference semantics here.  

#+ e
# (e) add a column, "probe_group" by reference based on first 5 digits of ID
chrom_t[ , `:=`(probe_group = str_sub(ID_REF, 1, 5))]

#' ## f
#' In part f, we compute the proportion of probes that nominally significant
#' differencs between the two Crohn's groups. "Nominally" here refers to the 
#' fact that we have not adjusted for
#'  [multiple comparisons](https://xkcd.com/882/).
#'  From the plot, we see that most of the chromosomes have more than the
#'  expected 5% differnces if the two groups were samples from populations with
#'  equal levels of methylation across all gens. However, chromosome 14 also 
#'  stands out as having a higher proportion of nominally significant probes
#'  than the others chromosomes

#+ f
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

#' ## g
#' Next, we would like to test the significance of the chromsome 14 anomaly noted
#' above. We will adjust our means for doing so by weighting probe sites with
#' larger t-values higher, but ascribing no signal to sites not passing the
#' nominal significance threshold. To do so, we will use a permutation test.
#' 
#' In this part, we write a function for computing the t-statistics and
#' chromsome/probe group summary scores as laid out in the assignment. The 
#' function will permute the group labels first if requested.
#' 
# (g) function for computimg group-level stat on original or permtued data: ----
compute_custom_stat = function(chrom_long, 
                               permute = FALSE,
                               type = c('two-tail', 'greater', 'lesser')
                      ) {
    # This function computes probe-level t-statistics and then aggregates them
    #   to chromosome level statistics. It is not intended for use outside of 
    #   this script.
    # Inputs:
    #  chrom_long - a data.table object, like the one of the same name
    #               created in this script above
    #  permute - when true, the sample groups are permuted prior to computing
    #            the t-statistics
    #  type - which statistic to compute, see the original assignment for
    #         details.
    # Output: A vector of 'probe_group'- (chromosome) level statistics. 

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

#' We could make the computations below more efficient by allowing the
#' function above to compute more than one permutation at a time. 
#'  

#' ## h
#' In this part, we compute the $T_{\textrm{abs}}$ statistic from 
#' 1000 permutations to build up a sample from a null distribution. We contrast
#' timings with one or two threads for data.table; this makes little difference
#' because the computations for a single permuation are relatively light.
#' 

#+
# (h) permute for T_abs, sequentially: -----------------------------------------

##Serial time for 1000 permutations, using two threads for data.table operations 
# getDTthreads()  ##2
nperm = 1e3
tm_seq =
  system.time({
  perm_list = vector(mode = 'list', length = nperm)
  for( perm in 1:nperm) {
    perm_list[[perm]] = compute_custom_stat(chrom_long, permute = TRUE)
    #if( perm %% 100 == 0) cat(perm, "perms\n")
  }
})

# Serial time for 1000 permutations using one thread for data.table operations
setDTthreads(threads = 1)
tm_seq1 = system.time({
  perm_list = vector(mode = 'list', length = nperm)
  for( perm in 1:nperm) {
    perm_list[[perm]] = compute_custom_stat(chrom_long, permute = TRUE)
    #if( perm %% 100 == 0) cat(perm, "perms\n")
  }
})

#' ## i
#' Next, we sample from the null distribution for the $T_{\textrm{up}}$ 
#' statistics.  Here we use `parallel::mclapply` to split the computations
#' into two parallel groups. The timing here is from my 4-core macbook. An
#' important point to realize here is that the argument `mc.preschedule` 
#' defaults to `TRUE`, so the process is only forked twice (not 1,000 times)
#' with each child process computing half of the function calls.

#+ i
# (i) permute for T_up, using mclapply: ----------------------------------------
# Note that dtThreads is still set to 1.
tm_mclapply = 
  system.time({
  perm_list_up = 
    parallel::mclapply(1:nperm, function(i) {
      compute_custom_stat(chrom_long, permute = TRUE, type = 'greater')
    }, mc.cores = 2)
})

#' ## j
#' In the final part, we obtain our 1,000 samples from the permutation 
#' distribution of $T_{\textrm{down}}$ using futures for parallelism. If you're
#' note careful to use pre-scheduling, you can make this run significantly 
#' slower than the sequential version by incurring the overhead of dispatching
#' 1,000 futures.  
#'
#' In timing the futures, it is also important that you don't stop timing until
#' all values are returned.  Otherwise it isn't comparable to the values above.

#+ j
# (j) permute for T_down, using futures: ---------------------------------------
# Note that dtThreads is still set to 1.
nworkers = 2
plan(multisession, workers = nworkers)

## Based on sequential timing, we know each function call takes ~0.04 seconds
##  we want to employ prescheduling manually to make this more feasible. 
##  Compare the result to issuing `nperm` futures in a simple for loop,
##  which would be like setting mc.preschedule = FALSE in mclapply.
tm_future = 
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
         compute_custom_stat(chrom_long, permute = TRUE, type = 'lesser')
        }
      )
    }) # ends the future
  } # ends the for loop

  # result above is a nested list of futures  
  perm_list_down = do.call("c", lapply(future_list, value) ) 
})

#' ## Timing Comparison
#' Here we briefly compare the timing of the implementations above. A solution
#' creating all multiple permutations in a single data.table would likely be
#' more efficient thans this.

#+ timings
cap_tm = "**Table 1.** *Timing comparisons.*"
data.table(
  Approach = c('Sequential, 2 dt threads',
               'Sequential, 1 dt threads',
               'mclapply, 2 forked process',
               'futures, 2 background chunks'),
  `Time, s` = round(rbind(tm_seq, tm_seq1, tm_mclapply, tm_future)[,3], 1)
  )%>%
  knitr::kable(format = 'html', caption = cap_tm) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

#' ## Results
#' We conclude by presenting the results.

#+ results
# A quick analysis of each set of tests: ---------------------------------------
## You weren't explicitly asked to do this, but you may want to see the results.

## two-tailed
chrom_perm_abs = rbindlist(perm_list)
chrom_obs = compute_custom_stat(chrom_long, permute = FALSE)
chrom_perm_abs = 
  merge(chrom_perm_abs, 
        chrom_obs[ , .(probe_group, obs = V1)],
        by = 'probe_group',
        all.x = TRUE
  )
p_abs = 
  chrom_perm_abs[ , .( p_abs = {1 + sum(V1 >= obs)} / {.N+1}), probe_group]

## higher methylation in Crohn's
chrom_perm_up = rbindlist(perm_list_up)
chrom_obs_up = compute_custom_stat(chrom_long, permute = FALSE, type = 'gr')
chrom_perm_up = 
  merge(chrom_perm_up, 
        chrom_obs_up[ , .(probe_group, obs = V1)],
        by = 'probe_group',
        all.x = TRUE
  )
p_up = 
  chrom_perm_up[ , .( p_up= {1 + sum(V1 >= obs)} / {.N+1}), probe_group]

## lower methylation in Crohn's
chrom_perm_down = rbindlist(perm_list_down)
chrom_obs_down = compute_custom_stat(chrom_long, permute = FALSE, type = 'le')
chrom_perm_down = 
  merge(chrom_perm_down, 
        chrom_obs_down[ , .(probe_group, obs = V1)],
        by = 'probe_group',
        all.x = TRUE
  )
p_down = 
  chrom_perm_down[ , .( p_down = {1 + sum(V1 <= obs)} / {.N+1}), probe_group]

p_tab = 
  merge( merge(p_abs, p_up, by = 'probe_group'), p_down, by = 'probe_group')
p_tab = p_tab[order(p_abs), lapply(.SD, round, digits = 3), 
              .SDcols = paste0('p_', c('abs', 'up', 'down')) ]

cap_res = "**Table 2.** *Results.*"
cn = c('$p_{\\textrm{abs}}$', '$p_{\\textrm{up}}$', '$p_{\\textrm{down}}$')
knitr::kable(p_tab, format = 'html', col.names = cn, caption = cap_res) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

