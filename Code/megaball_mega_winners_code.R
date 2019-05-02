
###################################
# Megaball simulations for winners
###################################

rm(list = ls())

# Dependencies
library(tidyverse)
library(readxl)
library(lubridate)

###############################
# Megaball simulation function
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

# Read in mega millions historical draws
mm_history = read_excel("~/Desktop/Lottery Research/Lottery Simulation Studies/mega_millions_history.xlsx")

# Munge to get winners
mm_nums = mm_history %>%
  mutate( Date = date(Date) ,
          payouts = as.numeric(gsub("[^0-9]", "", Jackpot)) ,
          winner = ifelse(payouts >= lag(payouts,1) , 1 , 0) ) %>%
  select(Date , One:Mega , winner)

##############################
# Pre-2013 Change: 56/46 balls
##############################

# Pre-2013 change
mm_jp_wins = mm_nums %>%
  filter(Date <= "2013-10-18") %>%
  filter(winner == 1) %>%
  select(Mega) %>%
  gather(Mega , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

zeros_data_frame = tibble( number = c(2,3,5,7,12,15,17,18,21,27,30,35,37,40,43,44) ,
                           n = rep(0,16) )

mm_jp_win_nos = bind_rows( mm_jp_win_nos_nozs , zeros_data_frame ) %>% arrange( number )

# Set ball number for examination of conscious selection
b = 23

# Plot winning number occurences for MM jackpots
plot( x = mm_jp_win_nos$number , y = mm_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )

#-------------------------#
# Simulations of MM Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
n_sims = 1e4
lottery_sims = lapply( rep(49,n_sims) , function(x) sim_lottos(x,ball_count = 46) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,46,1))
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
        mean(mm_jp_win_nos$n[mm_jp_win_nos$number <= b]) - mean(mm_jp_win_nos$n[mm_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( mm_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "1-46 Mega (before 10/2013)" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 49-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

###############################
# 2013-2017 Change: 75/15 balls
###############################

# Pre-2013 change
mm_jp_wins = mm_nums %>%
  filter(Date > "2013-10-18" & Date <= "2017-10-28") %>%
  filter(winner == 1) %>%
  select(Mega) %>%
  gather(Mega , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

mm_jp_win_nos = mm_jp_win_nos_nozs

# Set ball number for examination of conscious selection
b = 12

# Plot winning number occurences for MM jackpots
plot( x = mm_jp_win_nos$number , y = mm_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )

#-------------------------#
# Simulations of MM Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
n_sims = 1e4
lottery_sims = lapply( rep(32,n_sims) , function(x) sim_lottos(x,ball_count = 15) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,15,1))
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
        mean(mm_jp_win_nos$n[mm_jp_win_nos$number <= b]) - mean(mm_jp_win_nos$n[mm_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( mm_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  #geom_point( aes(x = n_avg - n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  xlim(min = 0 , max = 6) +
  labs( title = "1-15 Mega (10/2013 - 10/2017)" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 32-jp simulations" ) +
  ggthemes::theme_tufte()

###############################
# Post-2017 Change: 70/25 balls
###############################

# Post-2017 change
mm_jp_wins = mm_nums %>%
  filter(Date > "2017-10-28") %>%
  filter(winner == 1) %>%
  select(Mega) %>%
  gather(Mega , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

# 0 occurences added in (ball numbers with no jackpot occurences)
zeros_data_frame = tibble( number = c(2:4 , 6:9 , 11:13 , 15:19 , 22:25) ,
                           n = rep(0,19) )

# Append data to winning number occurences
mm_jp_win_nos = bind_rows( mm_jp_win_nos_nozs , zeros_data_frame ) %>% arrange( number )

# Set ball number for examination of conscious selection
b = 22

# Plot winning number occurences for MM jackpots
plot( x = mm_jp_win_nos$number , y = mm_jp_win_nos$n , type = "l")
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )
sd( mm_jp_win_nos$n[mm_jp_win_nos$number > b] )

#-------------------------#
# Simulations of MM Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
n_sims = 1e4
lottery_sims = lapply( rep(6,n_sims) , function(x) sim_lottos(x,ball_count = 25) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,25,1))
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
        mean(mm_jp_win_nos$n[mm_jp_win_nos$number <= b]) - mean(mm_jp_win_nos$n[mm_jp_win_nos$number > b]))/n_sims

#--------------------#
# Underpicked Numbers?
#--------------------#

all_number_sims = bind_rows( lottery_sims , .id = "simulation_number" )

# Add real results to simulated results
real_sims = all_number_sims %>% 
  group_by(number) %>%
  summarize( n_avg = mean(n) ,
             n_sd = sd(n) ) %>%
  full_join( mm_jp_win_nos , by = "number" )

# Visuzlization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  xlim(min = 0 , max = 1.5) +
  labs( title = "1-25 Mega (post-10/2017)" ,
        y = "Ball number (+2 SD bands)" ,
        x = "Average number of occurences in 6-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()
