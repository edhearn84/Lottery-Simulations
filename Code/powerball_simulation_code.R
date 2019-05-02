
#################################
# Powerball simulations for rolls
#################################

rm(list = ls())

# Dependencies
library(tidyverse)
library(readxl)
library(lubridate)

###############################
# Powerball simulation function
###############################

# Function for lottery draws over time span 01/07/2015 - 10/03/2015 (change ball_count for different era)
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

# Read in powerball historical draws
pb_history = read_excel("~/Desktop/Lottery Research/Lottery Simulation Studies/pb_history.xlsx")

# Munge to get winners
pb_nums = pb_history %>%
  mutate( Date = date(Date) ,
          payouts = as.numeric(gsub("[^0-9]", "", Jackpot)) ,
          winner = ifelse(payouts >= lag(payouts,1) , 1 , 0) ) %>%
  select(Date , One:PB , winner)

##############################
# Pre-2015 Change: 59/35 balls
##############################

# Pre-2015 change from 59 to 69
pb_jp_wins = pb_nums %>%
  filter(Date >= "2009-01-07" & Date <= "2015-10-03") %>%
  filter(winner == 0) %>%
  select(One:Five) %>%
  gather(One:Five , key = "Ball" , value = "number")

# Count of winning number occurrences
pb_jp_win_nos = pb_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

# Set ball number for examination of conscious selection
b = 29

# Plot winning number occurrences for PB jackpots
plot( x = pb_jp_win_nos$number , y = pb_jp_win_nos$n , type = "l" ,
      xlab = "Ball number" , ylab = "Occurrences" , main = "Real Series of Draws" , xaxt = "n")
axis(1 , seq(1,59,1))
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

# Empirical differences in means (pre-threshold and post-threshold)
real_diff_means = 
  mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] ) - mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

#-------------------------#
# Simulations of PB Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws
n_sims = 1e4
lottery_sims = lapply( rep(614,n_sims) , function(x) sim_lottos(x,ball_count = 59) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 6 counterfactuals)
draws = sample(n_sims , 6 , replace = FALSE)
par(mfrow = c(2,3))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = paste0("Simulated Series ",i) , xlab = "Ball number" , ylab = "Occurrences" , xaxt = "n" )
  axis(1 , seq(1,59,1))
  abline( v = b , lty = 2 , col = "red" )
}
 
par( mfrow = c(1,1) ) 

# Function to compute means pre- and post-29 ball (halfway in the 1 to 59 drum)
subtract_means = function(df) {
  mean( df$n[df$number >= b] ) - mean( df$n[df$number < b] )
}

# Individual draws
hist(sapply( lottery_sims , subtract_means ) , breaks = 30 ,
     main = paste0("Pre-",b,"-ball mean occurrence - post-",b,"-ball mean occurrence") , xlab = "Mean count difference")
abline(v = real_diff_means , lty = 2)

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
# average number of occurrences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg - 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  ggthemes::theme_tufte() +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "59-35 Powerball (01/2009 - 10/2015) Rolls" ,
        y = "Ball number (+/-2 SD bands)" ,
        x = "Average number of occurrences in 614-jp simulations\n(Red dots are actual occurrences)" ) +
  theme(text = element_text(size = 15))
  
###############################
# Post-2015 Change: 69/26 balls
###############################

# Post-2015 change from 59 to 69 balls
pb_jp_wins = pb_nums %>%
  filter(Date > "2015-10-03") %>%
  filter(winner == 0) %>%
  select(One:Five) %>%
  gather(One:Five , key = "Ball" , value = "number")

# Count of winning number occurrences
pb_jp_win_nos_nozs = pb_jp_wins %>%
  group_by(number) %>%
  summarize( n = n() )

# Append data to winning number occurrences
pb_jp_win_nos = pb_jp_win_nos_nozs

# Set ball number for examination of conscious selection
b = 34

# Plot winning number occurrences for PB jackpots
plot( x = pb_jp_win_nos$number , y = pb_jp_win_nos$n , type = "l" ,
      xlab = "Ball number" , ylab = "Occurrences" , main = "Real Series of Draws" , xaxt = "n")
axis(1 , seq(1,69,1))
abline( v = b , lty = 2 , col = "red" )

# Pre-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] )

# Post-halfway winning number mean/sd counts
mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )
sd( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

# Empirical differences in means (pre-threshold and post-threshold)
real_diff_means = 
  mean( pb_jp_win_nos$n[pb_jp_win_nos$number <= b] ) - mean( pb_jp_win_nos$n[pb_jp_win_nos$number > b] )

#-------------------------#
# Simulations of PB Results
#-------------------------#

# Simulate lottery draws for specific period of semi-weekly draws 
# (322 = number of roll jackpots since change)
n_sims = 1e4
lottery_sims = lapply( rep(322,n_sims) , function(x) sim_lottos(x,ball_count = 69) )

# Visualize lotteries as line graphs, overlay uniform distributions (random draw of 6 counterfactuals)
draws = sample(n_sims , 6 , replace = FALSE)
par(mfrow = c(2,3))
for ( i in 1:length(draws) ) {
  plot( x = lottery_sims[[i]]$number , y = lottery_sims[[i]]$n , type = "l",
        main = paste0("Simulated Series ",i) , xlab = "Ball number" , ylab = "Occurrences" , xaxt = "n" )
  axis(1 , seq(1,69,1))
  abline( v = b , lty = 2 , col = "red" )
}

par( mfrow = c(1,1) ) 

# Function to compute means pre- and post-34 ball (halfway in the 1 to 69 drum)
subtract_means = function(df) {
  mean( df$n[df$number >= b] ) - mean( df$n[df$number < b] )
}

# Individual draws
hist(sapply( lottery_sims , subtract_means ) , breaks = 30 ,
     main = paste0("Pre-",b,"-ball mean occurrence - post-",b,"-ball mean occurrence") , xlab = "Mean count difference")
abline(v = real_diff_means , lty = 2)

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
# average number of occurrences of ball numbers in jackpot-winning draws
real_sims %>%
  ggplot( aes(y = number , x = n_avg) ) +
  geom_text( aes( label = number ) , size = 3 ) +
  geom_point( aes(x = n_avg - 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n_avg + 2*n_sd , y = number) , pch = 124) +
  geom_point( aes(x = n , y = number) , col = "red" ) +
  theme( axis.ticks.y = element_blank() ,
         axis.text.y = element_blank() ) +
  labs( title = "69-26 Powerball (after 10/2015) Rolls" ,
        y = "Ball number (+2/-2 SD bands)" ,
        x = "Average number of occurrences in 322-jp simulations\n(Red dots are actual occurrences)" ) +
  ggthemes::theme_tufte() +
  theme(text = element_text(size = 15))

  
