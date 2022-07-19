#simulate 1 season
simulate_season <- function(df = scores_df){
  for (i in 1:nrow(df)) {
    if(is.na(df$H_score[i])){
      simulated_game <- simulate_game(df$Home[i], df$Away[i])
      df$H_score[i] <- simulated_game$H_goals[1]
      df$A_score[i] <- simulated_game$A_goals[1]
    }
  }
  
  df <- populate_wins_losses(df)
  simulated_table <- create_table(df)
  return(simulated_table)
}


