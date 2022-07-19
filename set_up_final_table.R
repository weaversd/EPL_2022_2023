#simulate season, variable from Run.R
simulation_table <- simulate_n_seasons(simulated_seasons)

#calculate the number of games played
played_games <- scores_df[!is.na(scores_df$H_score),]
played_games$Matchday <- as.numeric(played_games$Matchday)

#check to see if any games have been played. set # of MD to 1 if not
if (nrow(played_games) == 0) {
  current_matchday <- 1
} else {
  #determine the matchday
  #current_matchday <- max(as.numeric((played_games$Matchday)))
  current_matchday <- max(as.numeric(current_table$GP))
}

#set up a list to store results for each matchday
matchday_result_tables <- list()


#set up dataframe to track the position in the table for each matchday
positions <- data.frame(matrix(ncol = teams_n, nrow = matchweeks_total))
colnames(positions) <- team_list


#set defaults if current day is 1
if (current_matchday == 1){
  current_table$Prev <- NA
  current_table$Change <- 0
} else {
  #loop through each matchday, and subset the played games df to only include games up to that day
  for (i in 1:current_matchday) {
    matchday_result_tables[[i]] <- played_games[played_games$Matchday <= i,]
  }
  
  #create a table for each subset dataframe
  results_per_matchday <- list()
  for (i in 1:current_matchday) {
    results_per_matchday[[i]] <- create_table(matchday_result_tables[[i]])
  }
  
  #store each teaams position for each matchday in the dataframe
  for (i in 1:current_matchday) {
    for (j in 1:teams_n) {
      team_name <- team_list[[j]]
      matchday_table <- results_per_matchday[[i]]
      positions[[team_name]][i] <- matchday_table[matchday_table$Team == team_name,][["Pos"]]
    }
  }
  
  #set previous matchday
  current_table$Prev <- NA
  previous_MD <- current_matchday - 1
  
  #set previous position
  for (j in 1:teams_n) {
    current_team <- current_table$Team[j]
    current_table$Prev[j] <- positions[[current_team]][previous_MD]
  }
  
  #calculate change in position
  current_table$Change <- current_table$Prev - current_table$Pos
}

#calculate finishing positions from all simulated seasons
current_table$UCL_pct <- NA
current_table$Prem_Title_pct <- NA
current_table$Relegated_pct <- NA
for (j in 1:teams_n) {
  #from all simulated seasons, pull out percentages and store in current table 
  current_team <- current_table$Team[j]
  current_table$UCL_pct[j] <- simulation_table[simulation_table$Team == current_team,][['UCL_pct']]
  current_table$Prem_Title_pct[j] <- simulation_table[simulation_table$Team == current_team,][['Prem_Title_pct']]
  current_table$Relegated_pct[j] <- simulation_table[simulation_table$Team == current_team,][['Relegated_pct']]
}

#round values
current_table$UCL_pct <- round(current_table$UCL_pct, 2)
current_table$Prem_Title_pct <- round(current_table$Prem_Title_pct, 2)
current_table$Relegated_pct <- round(current_table$Relegated_pct, 2)



#invert the position dataframe, so the line graphs look right
inv_positions <- (teams_n+1) - positions

#set up sparkline dataframe
sparklines <- rep(NA, teams_n)

#set up sparklines for each team
for (j in 1:teams_n) {
  current_team <- current_table$Team[j]
  #for each team, make a sparkline with position data from inv_positions
  sparklines[j] <- as.character(htmltools::as.tags(sparkline(inv_positions[[current_team]], type = "line",
                                                             chartRangeMin = 0.8, chartRangeMax = 20.1,
                                                             fillColor = FALSE,
                                                             minSpotColor = "",
                                                             maxSpotColor = "",
                                                             spotColor = "green",
                                                             lineWidth = 1,
                                                             normalRangeMin = 0.9,
                                                             normalRangeMax = 20.1,
                                                             disableInteraction = T,
                                                             disableTooltips = T,
                                                             disableHighlight = T,
                                                             normalRangeColor = "lightgrey")))
}

#add sparklines to table
current_table$`Pos_trend` <- sparklines

#reorder current table rows
current_table <- current_table[order(-current_table$Pts, -current_table$GD,
                                     -current_table$GF, -current_table$UCL_pct),]

#reorder current table columns
current_table <- current_table[,c(1, 10, 11, 12, 16, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15)]


#set up the table with formattable. add colors and bars and sparklines
table_out <- as.htmlwidget(formattable(current_table, row.names = F,
                                       align = c("l", "c", "r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
                                       list(`Pts`= color_bar("lightblue"),
                                            `GD` = color_tile("pink", "lightgreen"),
                                            `GF` = color_bar("lightgreen"),
                                            `GA` = color_bar("pink"),
                                            `W` = color_bar("lightgreen"),
                                            `L` = color_bar("pink"),
                                            `D` = color_bar("khaki"),
                                            `UCL_pct` = color_bar("lightgreen"),
                                            `Prem_Title_pct` = color_bar("#36d160"),
                                            `Relegated_pct` = color_bar("pink"),
                                            `Prev` = FALSE,
                                            `Change`= formatter("span",
                                                                style = ~ style(color = ifelse(`Prev` == `Pos`, "black", ifelse(`Prev` > `Pos`, "green", "red"))),
                                                                ~ icontext(sapply(`Change`, function(x) if (x < 0) "arrow-down" else if (x > 0) "arrow-up" else ""), `Change`)))))

#make the sparklines work
table_out$dependencies = c(table_out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
table_out

