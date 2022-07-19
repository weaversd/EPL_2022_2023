simulate_n_seasons <- function(iterations = 10) {
  
  #set up progress bar
  print(paste0("simulating ", iterations, " seasons..."), quote = F)
  print("[----------]", quote = F)
  teams_results <- list()
  for (j in 1:teams_n){
    teams_results[[j]] <- data.frame()
  }
  
  ten <- iterations %/% 10
  twenty <- ten * 2
  thirty <- ten * 3
  forty <- ten * 4
  fifty <- ten * 5
  sixty <- ten * 6
  seventy <- ten * 7
  eighty <- ten * 8
  ninety <- ten * 9
  
  for (count in 1:iterations) {
    if (count == ten) {
      print("[*---------]", quote = F)
    } else if (count == twenty){
      print("[**--------]", quote = F)
    } else if (count == thirty) {
      print("[***-------]", quote = F)
    } else if (count == forty) {
      print("[****------]", quote = F)
    } else if (count == fifty) {
      print("[*****-----]", quote = F)
    } else if (count == sixty) {
      print("[******----]", quote = F)
    } else if (count == seventy) {
      print("[*******---]", quote = F)
    } else if (count == eighty) {
      print("[********--]", quote = F)
    } else if (count == ninety) {
      print("[*********-]", quote = F)
    }
    

    season_results <- simulate_season()

    #bind all of the results for one team together
    for (j in 1:teams_n){
      teams_results[[j]] <- rbind(teams_results[[j]], season_results[season_results$Team == team_list[j],])
    }
    
  }
  print("[**********]", quote = F)
  print(paste0("completed ", iterations, " simulations (100%)"), quote = F)
  print("compiling...", quote = F)
  
  #create a summary dataframe
  summary_dataframe <- data.frame(matrix(ncol = 13, nrow = teams_n))
  
  #rename columns
  names <- c("Team", "GP", "W", "L", "D", "Pts", "GF",
             "GA", "GD", "Pos", "UCL_pct", "Prem_Title_pct", "Relegated_pct")
  colnames(summary_dataframe) <- names
  
  #for each team summarise. Average statistics, and count top4 finishes, top finishes, and relegations
  for(j in 1:teams_n) {
    team_df <- teams_results[[j]]
    summary_dataframe$Team[j] <- team_df$Team[1]
    summary_dataframe$GP[j] <- mean(team_df$GP)
    summary_dataframe$W[j] <- mean(team_df$W)
    summary_dataframe$L[j] <- mean(team_df$L)
    summary_dataframe$D[j] <- mean(team_df$D)
    summary_dataframe$Pts[j] <- mean(team_df$Pts)
    summary_dataframe$GF[j] <- mean(team_df$GF)
    summary_dataframe$GA[j] <- mean(team_df$GA)
    summary_dataframe$GD[j] <- mean(team_df$GD)
    summary_dataframe$Pos[j] <- mean(team_df$Pos)
    ucl_count <- sum(team_df$Pos <= 4)
    ucl_pct <- ucl_count / iterations
    title_count <- sum(team_df$Pos == 1)
    title_pct <- title_count / iterations
    relegate_count <- sum(team_df$Pos >= 18)
    relegate_pct <- relegate_count / iterations
    summary_dataframe$UCL_pct[j] <- ucl_pct *100
    summary_dataframe$Prem_Title_pct[j] <- title_pct *100
    summary_dataframe$Relegated_pct[j] <- relegate_pct *100
    
  }
  
  #order the dataframe
  ordered_summary_df <- summary_dataframe[order(-summary_dataframe$Pts,
                                                -summary_dataframe$GD,
                                                -summary_dataframe$GF),]
  
  #return ordered df
  return(ordered_summary_df)
  
  
}
