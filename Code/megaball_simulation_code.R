
################################
# Megaball simulations for rolls
################################

rm(list = ls())

# Dependencies
library(tidyverse)
library(readxl)
library(lubridate)

###############################
# Megaball simulation function
###############################

# Function for lottery draws over time span 2010-present (change ball_count for different eras)
# Different eras: (pre-10/18/2013) - 5/56 & 1/46
#                 (10/18/2013 - 10/28/2017) - 5/75 & 1/15
#                 (post-10/28/2017) - 5/70 & 1/25

# Function for powerball draws (1-ball)
sim_lottos = function(tot_draws , ball_count = 59){
  
  lotto_draws = array( NA , c(tot_draws,5) )
  
  for ( i in 1:nrow(lotto_draws) ) {
    lotto_draws[i,] = sample(1:ball_count , 5 , replace = FALSE)
  }
  
  lotto_draws_summary = data.frame(lotto_draws)
  colnames(lotto_draws_summary) = c("Ball_1","Ball_2","Ball_3","Ball_4","Ball_5")
  
  balls = tibble(number = 1:ball_count)
  
  lotto_nums_count = lotto_draws_summary %>%
    gather(Ball_1:Ball_5 , key = Ball_no , value = number) %>%
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
  filter(winner == 0) %>%
  select(One:Five) %>%
  gather(One:Five , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

mm_jp_win_nos = mm_jp_win_nos_nozs

# Set ball number for examination of conscious selection
b = 48

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
# 339 = number of won jackpots
n_sims = 1e4
lottery_sims = lapply( rep(339,n_sims) , function(x) sim_lottos(x,ball_count = 56) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,56,1))
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
  geom_point( aes(x = n_avg - 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "56-46 Mega Millions Rolls" ,
        y = "Ball number (+2/-2 SD bands)" ,
        x = "Average number of occurences in 339-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

##########################################
# 10/18/2013 - 10/28/2017 Era: 75/15 balls
##########################################

# 2013-2017 era
mm_jp_wins = mm_nums %>%
  filter(Date > "2013-10-18" & Date <= "2017-10-28") %>%
  filter(winner == 0) %>%
  select(One:Five) %>%
  gather(One:Five , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

mm_jp_win_nos = mm_jp_win_nos_nozs

# Set ball number for examination of conscious selection
b = 67

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
# 388 = number of won jackpots
n_sims = 1e4
lottery_sims = lapply( rep(388,n_sims) , function(x) sim_lottos(x,ball_count = 75) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,75,1))
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

# Visualization of jackpot results: 
# average number of occurences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg - 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "75-15 Mega Millions Rolls" ,
        y = "Ball number (+2/-2 SD bands)" ,
        x = "Average number of occurences in 388-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()

##########################################
# Post- 10/28/2017 Era: 70/25 balls
##########################################

# Post-10/27/18 era
mm_jp_wins = mm_nums %>%
  filter(Date > "2017-10-28") %>%
  filter(winner == 0) %>%
  select(One:Five) %>%
  gather(One:Five , key = "Ball" , value = "number")

# Count of winning number occurences
mm_jp_win_nos_nozs = mm_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

mm_jp_win_nos = mm_jp_win_nos_nozs

# Set ball number for examination of conscious selection
b = 65

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
# 125 = number of won jackpots
n_sims = 1e4
lottery_sims = lapply( rep(125,n_sims) , function(x) sim_lottos(x,ball_count = 70) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 8 counterfactuals)
draws = sample(n_sims , 8 , replace = FALSE)
par(mfrow = c(2,4))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = draws[i] , xlab = "" , ylab = "freq" , xaxt = "n" )
  axis(1 , seq(1,70,1))
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
  geom_point( aes(x = n_avg - 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  xlim(min = 0 , max = 20) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "70-25 Mega Millions Rolls" ,
        y = "Ball number (+2/-2 SD bands)" ,
        x = "Average number of occurences in 125-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte()
