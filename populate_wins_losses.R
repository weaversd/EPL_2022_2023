#populate winners and losers
populate_wins_losses <- function(df = scores_df) {
  for (i in 1:nrow(df)) {
    if(!is.na(df$H_score[i]) & (df$H_score[i] == df$A_score[i])){
      df$Winner[i] <- "Draw"
      df$Loser[i] <- "Draw"
      df$Draw[i] <- T
    } else if (!is.na(df$H_score[i]) & (df$H_score[i] != df$A_score[i])){
      df$Draw[i] <- F
      if (df$H_score[i] > df$A_score[i]){
        df$Winner[i] <- df$Home[i]
        df$Loser[i] <- df$Away[i]
      } else {
        df$Winner[i] <- df$Away[i]
        df$Loser[i] <- df$Home[i]
      }
    }
  }
  return(df)
}
