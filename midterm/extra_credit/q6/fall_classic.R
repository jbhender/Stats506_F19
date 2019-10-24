# The Riddler's Fall Classic, (Stats 506 Midterm, Question 6)
# https://fivethirtyeight.com/features/which-baseball-team-will-win-the-riddler-fall-classic/
#
# Background: 
#   Three hypothetical baseball teams with each batter having one of four
#   outcomes: 
#      BB - a walk granting one base and advancing those on base 1 position,
#      DB - a double with the batter getting to 2nd base and any players on 
#           base scoring,
#      HR - a home run, the batter and anyone on base scores,
#       K - a strike out, the batter is out.
# 
#   Team 1 (MW): P(BB = .4), P(K = .6)
#   Team 2 (DD): P(DB = .2), P(K = .8)
#   Team 3 (TT): P(HR = .1), P(K = .9)
#
#  Notes: The number of batters reaching base by each team in an inning follows a 
#  Negative Binomial distribution. The number of runs scored is a funciton of
#  this number.  

# Original question: If these teams play in a baseball season (162 games, or 81
#  games between each pair of teams), which team wins the most games on average? 

# libraries: ------------------------------------------------------------------
library(tidyverse)

# Functions to simulate a inning or nine: -------------------------------------
sim_inning = function(n = 1, team = c('mw', 'dd', 'tt')){
  # simulates the number of runs scored by "team" in a single inning
  # inputs: 
  #   team - one of 'mw', 'dd', or 'tt' 
  #   n - an integer for the number of innings to simulate
  # outputs: a length n numeric vector with a simulated number of runs scored
  #          for each of n innings.
  team = match.arg(team)
  
  if ( team == 'mw' ) {
   # number of batters that walk before there are three outs
   # the fourth and each subsequent walk scores a run
   return( pmax( rnbinom(n, 3, .6) - 3, 0) )
  }
  
  if ( team == 'dd' ) {
    # number of batters that double before there are three outs
    # the second and subsequent doubles score runs
    return( pmax( rnbinom(n, 3, .8) - 1, 0) )
  }
  
  if ( team == 'tt' ) {
    # the number of batters that hit home runs before there are three outs
    # each homerun scores a run
    return( rnbinom(n, 3, .9) )
  }
  
}

sim_nine = function( n = 1, team = c('mw', 'dd', 'tt') ) {
  # simulates the number of runs score in the first nine innings
  # for one or more games of "riddler league" baseball
  # inputs: 
  #    n - the number of games to simulate, an integer
  #    team - one of 'mw', 'dd', or 'tt' 
  # outputs: a length n numeric vector, representing a simulated number of runs 
  #          in the first 9 innings of n games
  
  team = match.arg(team)
  
  #Simulate runs score in 9 inning games.
  #<Task 1> Improve this block of code using vectorization. 
  runs = matrix(NA, 9, n)
  for ( game in 1:n ){
    for ( inning in 1:9 ){
      runs[inning, game] = sim_inning(n = 1, team = team)
    }
  }
  apply(runs, 2, sum)
  
}

extras = function(team1, team2) {
  # This function simulates the outcome for games tied after nine innings.
  # Inputs: 
  #   team1, team2 - character strings giving the teams to simulate the game
  #    outcome for, conditional on the game being tied after 9 innings.
  #   each in "mw", "dd", or "tt"
  # Outputs: the name of the team that wins, either team1 or team2.
  r1 = function() eval( call( paste0('sim_inning_', team1 ) ) ) 
  r2 = function() eval( call( paste0('sim_inning_', team2 ) ) )
  
  while ( TRUE ) {
    top = sim_inning( n=1, team=team1)
    bottom = sim_inning( n=1, team =team2)
    if ( top > bottom ) {
      return(team1)
    } else if ( top < bottom ) {
      return(team2)
    }
  }
}

# Generate a Monte Carlo sample of games: -------------------------------------
mcrep = 1e4
mchalf = mcrep / 2
games_mw = sim_nine(mcrep, team = 'mw')
games_dd = sim_nine(mcrep, team = 'dd')
games_tt = sim_nine(mcrep, team = 'tt')

# Estimate the average number of runs scored per game by each team: -----------
#! <Task 2> 

# Compute the approximate winning probabilities: ------------------------------
# MW vs DD
mw_vs_dd = ifelse( games_mw[1:mchalf] > games_dd[1:mchalf], 'mw', 'dd')
mw_vs_dd_ties = which( games_mw[1:mchalf] == games_dd[1:mchalf] )
mw_vs_dd[ mw_vs_dd_ties ] = 
  sapply( mw_vs_dd_ties, function(i) extras('mw', 'dd') )

# MW vs TT
mw_vs_tt = ifelse( games_mw[{mchalf + 1}:mcrep] > games_tt[1:mchalf],
                   'mw', 'tt')
mw_vs_tt_ties = which( games_mw[{mchalf + 1}:mcrep] == games_tt[1:mchalf] )
mw_vs_tt[ mw_vs_tt_ties ] = 
  sapply( mw_vs_tt_ties, function(i) extras('mw', 'tt') )

# DD vs TT
dd_vs_tt = ifelse( games_dd[{mchalf + 1}:mcrep] > games_tt[{mchalf + 1}:mcrep],
                   'dd', 'tt')
dd_vs_tt_ties =
  which( games_dd[{mchalf + 1}:mcrep] == games_tt[{mchalf + 1}:mcrep] )
dd_vs_tt[ dd_vs_tt_ties ] = 
  sapply( dd_vs_tt_ties, function(i) extras('dd', 'tt') )

## Expected winning totals in a 162 game season for each team: -----------------
#! <Task 3>
mw_wins = 
dd_wins = 
tt_wins = 



