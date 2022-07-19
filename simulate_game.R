simulate_game <- function(team_H, team_A) {
  #import offensive and defensive SPI rankings, with 20% boost for home team offense
  team_H_off <- as.numeric(SPI_df[SPI_df$Name == team_H,]$Off) * home_field_advantage
  team_H_def <- as.numeric(SPI_df[SPI_df$Name == team_H,]$Def)
  
  team_A_off <- as.numeric(SPI_df[SPI_df$Name == team_A,]$Off)
  team_A_def <- as.numeric(SPI_df[SPI_df$Name == team_A,]$Def)
  
  
  #caluclate predicted goals for each team
  team_H_expect <- mean(c(team_H_off, team_A_def))
  team_A_expect <- mean(c(team_A_off, team_H_def))
  
  
  #create a percent poisson distribution for home team, from 0 to 5 goals
  H_pois_raw <- rep(0,5)
  for (i in 0:4) {
    H_pois_raw[i+1] <- ppois(i, lambda = team_H_expect)
  }
  
  #generate a random number for home score
  H_random <- runif(1)
  
  #take the distribution and the random number and simulate a number of goals
  H_goals <- 0
  for (i in 1:5) {
    if (H_random > H_pois_raw[i]) {
      H_goals <- i
    }
  }
  
  
  
  #create a percent poisson distribution for away team, from 0 to 5 goals
  A_pois_raw <- rep(0,5)
  for (i in 0:4) {
    A_pois_raw[i+1] <- ppois(i, lambda = team_A_expect)
  }
  
  #generate a random number for away score
  A_random <- runif(1)
  
  #take the distribution and the random number and simulate a number of goals
  A_goals <- 0
  for (i in 1:5) {
    if (A_random > A_pois_raw[i]) {
      A_goals <- i
    }
  }
  
  #put the scores into a df
  simulated_score_df <- data.frame(H_goals = H_goals, A_goals = A_goals)
  
  #return the dataframe
  return(simulated_score_df)
}
