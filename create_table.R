#function to take the scores_df and only select games with one team
subset_by_team <- function(team, df = scores_df) {
  team_df <- df[(df$Home == team) | (df$Away == team),]
  return(team_df)
}


#create table
create_table <- function(df = scores_df) {
  #set variables to empty to start
  Team <- rep("", teams_n)
  GP <- rep(0, teams_n)
  W <- rep(0, teams_n)
  L <- rep(0, teams_n)
  D <- rep(0, teams_n)
  Pts <- rep(0, teams_n)
  GF <- rep(0, teams_n)
  GA <- rep(0, teams_n)
  GD <- rep(0, teams_n)
  counter <- 1
  
  #loop through each team
  for (team in team_list) {
    
    #subset by team
    temp_team <- subset_by_team(team, df = df)
    
    #create a dataframe where only the played games are listed
    temp_team_played <- temp_team[!is.na(temp_team$H_score),]
    
    #check to ensure at least one game is played, if not set all values to zero
    if (nrow(temp_team_played) == 0){
      games_played <- 0
      temp_draws <- 0
      temp_wins <- 0
      temp_losses <- 0
      temp_losses <- sum(temp_team_played$Loser == team)
      temp_points <- (3*temp_wins) + (temp_draws)
      temp_goals_for <- 0
      temp_goals_against <- 0
      temp_goal_diff <- temp_goals_for - temp_goals_against
    } else {
      games_played <- nrow(temp_team_played)
      temp_draws <- sum(temp_team_played$Draw)
      temp_wins <- sum(temp_team_played$Winner == team)
      temp_losses <- sum(temp_team_played$Loser == team)
      temp_points <- (3*temp_wins) + (temp_draws)
      
      #set goals for and against to zero
      temp_goals_for <- 0
      temp_goals_against <- 0
      
      #count goals for and against
      for (i in 1:nrow(temp_team_played)) {
        if (temp_team_played$Home[i] == team) {
          temp_goals_for <- temp_goals_for + temp_team_played$H_score[i]
          temp_goals_against <- temp_goals_against + temp_team_played$A_score[i]
        } else {
          temp_goals_for <- temp_goals_for + temp_team_played$A_score[i]
          temp_goals_against <- temp_goals_against + temp_team_played$H_score[i]
        }
      }
      
      #calculate goal diff
      temp_goal_diff <- temp_goals_for - temp_goals_against
    }
    
    #populate dataframe
    Team[counter] <- team
    GP[counter] <- games_played
    W[counter] <- temp_wins
    L[counter] <- temp_losses
    D[counter] <- temp_draws
    Pts[counter] <- temp_points
    GF[counter] <- temp_goals_for
    GA[counter] <- temp_goals_against
    GD[counter] <- temp_goal_diff
    
    #iterate
    counter <- counter + 1
  }
  
  #create table, and order by points, goal differential and goals for
  table <- data.frame(Team, GP, W, L, D, Pts, GF, GA, GD)
  ordered_table <- table[order(-Pts, -GD, -GF),]
  ordered_table$Pos <- seq.int(nrow(ordered_table))
  return(ordered_table)
}
