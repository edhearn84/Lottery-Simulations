
###################################
# Powerball simulations for winners
###################################

rm(list = ls())

# Dependencies
library(tidyverse)
library(readxl)
library(lubridate)

###############################
# Powerball simulation function
###############################

# Function for powerball draws (1-ball)
sim_lottos = function(tot_draws , ball_count = 39){
  
  lotto_draws = array( NA , c(tot_draws,1) )
  
  for ( i in 1:nrow(lotto_draws) ) {
    lotto_draws[i,] = sample(1:ball_count , 1 , replace = FALSE)
  }
  
  lotto_draws_summary = data.frame(lotto_draws)
  colnames(lotto_draws_summary) = c("PB")
  
  balls = tibble(number = 1:ball_count)
  
  lotto_nums_count = lotto_draws_summary %>%
    gather(PB , key = Ball_no , value = number) %>%
    group_by(number) %>%
    summarize(n = n()) %>%
    full_join(balls , by = "number") %>% 
    mutate( n = ifelse(is.na(n) , 0 , n) ) %>% 
    arrange(number) 
  
  return(lotto_nums_count)
}

#################################
# Data on Winning/Rolling Numbers
#################################

# Read in powerball historical draws
pb_history = read_excel("~/Desktop/Lottery Research/Lottery Simulation Studies/pb_history.xlsx")

# Munge to get winners
pb_nums = pb_history %>%
  mutate( Date = date(Date) ,
          payouts = as.numeric(gsub("[^0-9]", "", Jackpot)) ,
          winner = ifelse(payouts >= lag(payouts,1) , 1 , 0) ) %>%
  select(Date , One:PB , winner)

####################################
# Pre-2012 Change: 1-39 Powerball
####################################

# Pre-2012 change: 39 powerball draw
pb_jp_wins = pb_nums %>%
  filter(Date >= "2009-01-07" & Date <= "2012-01-14") %>%
  filter(winner == 1) %>%
  select(PB) %>%
  gather(PB, key = "Ball" , value = "number")

# Count of winning number occurences
pb_jp_win_nos_nozs = pb_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

zeros_data_frame = tibble( number = c(1,3,12,14,16,18,21,25,28,32,34,35,39) ,
                           n = rep(0,13) )

pb_jp_win_nos = bind_rows( pb_jp_win_nos_nozs , zeros_data_frame ) %>% arrange( number )

# Set ball number for examination of conscious selection
b = 34

# Plot winning number occurences for PB jackpots
plot( x = pb_jp_win_nos$number , y = pb_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

#-------------------------#
# Simulations of PB Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
# for 40 winners
n_sims = 1e4
lottery_sims = lapply( rep(40,n_sims) , function(x) sim_lottos(x,ball_count = 39) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,39,1))
}

par( mfrow = c(1,1) ) 

# Function to compute means pre- and post-29 ball (halfway in the 1 to 59 drum)
subtract_means = function(df) {
  mean( df$n[df$number >= b] ) - mean( df$n[df$number < b] )
}

# Individual draws
hist(sapply( lottery_sims , subtract_means ) ,
     main = paste0("Pre-",b," mean draw amount - post-",b," mean draw amount") , xlab = "Mean count difference")

# Mean, variance, and quantiles of difference in "low-number" and "high-number" jackpot simulations
mean( sapply( lottery_sims , subtract_means ) )
var( sapply( lottery_sims , subtract_means ) )
quantile( sapply( lottery_sims , subtract_means ) )

# Winning number draws random? 
table(sapply(lottery_sims , subtract_means) > 
        mean(pb_jp_win_nos$n[pb_jp_win_nos$number <= b]) - mean(pb_jp_win_nos$n[pb_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( pb_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "1-39 Powerball (before 01/2012) Wins" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 40-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

##############################
# Pre-2015 Change: 1-35 balls
##############################

# Pre-2015 change to 1-35 powerball draw
pb_jp_wins = pb_nums %>%
  filter(Date >= "2012-01-14" & Date <= "2015-10-03") %>%
  filter(winner == 1) %>%
  select(PB) %>%
  gather(PB, key = "Ball" , value = "number")

# Count of winning number occurences
pb_jp_win_nos_nozs = pb_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

zeros_data_frame = tibble( number = c(1,5,8,16,20,22,33,35) ,
                           n = rep(0,8) )

pb_jp_win_nos = bind_rows( pb_jp_win_nos_nozs , zeros_data_frame ) %>% arrange( number )

# Set ball number for examination of conscious selection
b = 32

# Plot winning number occurences for PB jackpots
plot( x = pb_jp_win_nos$number , y = pb_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

#-------------------------#
# Simulations of PB Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
# for 50 winners
n_sims = 1e4
lottery_sims = lapply( rep(50,n_sims) , function(x) sim_lottos(x,ball_count = 35) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,35,1))
}

par( mfrow = c(1,1) ) 

# Function to compute means pre- and post-34 ball (halfway in the 1 to 69 drum)
subtract_means = function(df) {
  mean( df$n[df$number >= b] ) - mean( df$n[df$number < b] )
}

# Individual draws
hist(sapply( lottery_sims , subtract_means ) ,
     main = paste0("Pre-",b," mean draw amount - post-",b," mean draw amount") , xlab = "Mean count difference")

# Mean, variance, and quantiles of difference in "low-number" and "high-number" jackpot simulations
mean( sapply( lottery_sims , subtract_means ) )
var( sapply( lottery_sims , subtract_means ) )
quantile( sapply( lottery_sims , subtract_means ) )

# Winning number draws random? 
table(sapply(lottery_sims , subtract_means) > 
        mean(pb_jp_win_nos$n[pb_jp_win_nos$number <= b]) - mean(pb_jp_win_nos$n[pb_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( pb_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "1-35 Powerball (01/2012 - 10/2015) Wins" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 50-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

##############################
# Post-2015 Change: 1-26 balls
##############################

# Post-2015 change to 1-26 powerball draw
pb_jp_wins = pb_nums %>%
  filter(Date > "2015-10-03") %>%
  filter(winner == 1) %>%
  select(PB) %>%
  gather(PB, key = "Ball" , value = "number")

# Count of winning number occurences
pb_jp_win_nos_nozs = pb_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

zeros_data_frame = tibble( number = c(11,12,15,18,19,20,21,22,23) ,
                           n = rep(0,9) )

pb_jp_win_nos = bind_rows( pb_jp_win_nos_nozs , zeros_data_frame ) %>% arrange( number )

# Set ball number for examination of conscious selection
b = 23

# Plot winning number occurences for PB jackpots
plot( x = pb_jp_win_nos$number , y = pb_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

#-------------------------#
# Simulations of PB Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
# for 23 winners
n_sims = 1e4
lottery_sims = lapply( rep(23,n_sims) , function(x) sim_lottos(x,ball_count = 26) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,26,1))
}

par( mfrow = c(1,1) ) 

# Function to compute means pre- and post-34 ball (halfway in the 1 to 69 drum)
subtract_means = function(df) {
  mean( df$n[df$number >= b] ) - mean( df$n[df$number < b] )
}

# Individual draws
hist(sapply( lottery_sims , subtract_means ) ,
     main = paste0("Pre-",b," mean draw amount - post-",b," mean draw amount") , xlab = "Mean count difference")

# Mean, variance, and quantiles of difference in "low-number" and "high-number" jackpot simulations
mean( sapply( lottery_sims , subtract_means ) )
var( sapply( lottery_sims , subtract_means ) )
quantile( sapply( lottery_sims , subtract_means ) )

# Winning number draws random? 
table(sapply(lottery_sims , subtract_means) > 
        mean(pb_jp_win_nos$n[pb_jp_win_nos$number <= b]) - mean(pb_jp_win_nos$n[pb_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( pb_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "1-26 Powerball (post-10/2015) Wins" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 23-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

