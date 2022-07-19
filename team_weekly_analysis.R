TEAM <- "Arsenal"

create_team_plot <- function(TEAM) {
  weekly_table_norm <- weekly_table
  for (i in 1:ncol(weekly_table_norm)) {
    if (typeof(weekly_table_norm[[i]]) != "character" & !is.factor(weekly_table_norm[[i]])){
      if (grepl("pct", names(weekly_table_norm)[i])) {
        #divide by 100
        weekly_table_norm[[i]] <- weekly_table[[i]]/100
      } else if (i != 16){ #don't normalize matchday
        weekly_table_norm[[i]] <- normalize(weekly_table[[i]], na.rm = T)
      }
    }
  }
  
  #select one team and pull the rows out
  TEAM_df_norm <- weekly_table_norm[weekly_table_norm$Team == TEAM,]
  TEAM_df <- weekly_table[weekly_table$Team == TEAM,]
  
  #reverse the order of position so higher is worse
  TEAM_df_norm$Pos <- 1 - TEAM_df_norm$Pos
  
  
  TEAM_df_long <- gather(TEAM_df_norm, attribute, value, GF, GA, Pts, W, L, D,
                         UCL_pct, Relegated_pct, Prem_Title_pct, GD, Pos)
  
  TEAM_df_long$attribute <- factor(TEAM_df_long$attribute,
                                   levels = c("Pos","Pts","W", "L", "D",
                                              "GF", "GA", "GD",
                                              "UCL_pct",
                                              "Prem_Title_pct",
                                              "Relegated_pct"))
  
  
  team_plot <- ggplot(data = TEAM_df_long) +
    geom_line(aes(x = matchday, y = value, color = attribute, linetype = attribute), size = 1.25) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 20),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          legend.key.width = unit(1.5,"cm"),
          plot.title = element_text(size = 90)) +
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
    labs(x = "Week", y = "Normalized units OR %",
         title = paste0(TEAM)) +
    scale_y_continuous(breaks = c(0,1), limits = c(0,1)) +
    scale_color_manual(values = c("blue", "blue4",
                                  "darkgreen", "red4", "orange",
                                  "lightgreen", "hotpink", "skyblue",
                                  "green", "green4", "red")) +
    scale_linetype_manual(values = c("twodash", "solid",
                                     "dashed", "dashed", "dashed",
                                     "dotted", "dotted", "dotted",
                                     "dotdash", "dotdash", "dotdash"))
    #guides(color = guide_legend(override.aes = list(size = 5)))
  return(team_plot)
}


for (i in 1:length(team_list)){
  plot <- create_team_plot(team_list[i])
  show(plot)
  
  png(paste0("weekly_analysis/weekly_team_plots/", team_list[i], "_by_week.png"), width = 1200, height = 700)
  show(plot)
  dev.off()
}
