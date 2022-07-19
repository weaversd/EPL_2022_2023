#import each matchweek table
matchweek_tables_SPI <- list()
matchweek_files_SPI <- list.files(path = "SPI_tables/")

#save each to a different index in the list
for (i in 1:length(matchweek_files_SPI)) {
  matchweek_tables_SPI[[i]]<- read.csv(paste0("SPI_tables/", matchweek_files_SPI[i]))
  matchweek_tables_SPI[[i]]$matchday <- i
}

#combine into one df
weekly_table_SPI <- bind_rows(matchweek_tables_SPI)


#create a color dictionary

color_list <- c("#EF0107", "#670e36", "#000000", "#A0D5CB", "#fde3ab",
                "#034694", "#a7a5a6", "#87BDA0", "#AFAAE2", "#003A83",
                "#009782", "#97c1e7", "#DA291C", "#523a28", "#00A650",
                "#525252", "#132257", "#fbee23", "#7f0000", "#fdb913")
colors <- data.frame(row.names = team_list, color = color_list)

#to look at the colors for testing:
# color_test_df <- data.frame(teams = team_list,
#                             number = 1:20,
#                             color = color_list)
# 
# test <- ggplot(data = color_test_df) +
#   geom_point(aes(x = teams, y = number), size = 20, color = color_test_df$color) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# test




#add colors to the table
weekly_table_SPI$color <- NA
for (i in 1:nrow(weekly_table_SPI)) {
  team_name <- weekly_table_SPI$Name[i]
  weekly_table_SPI$color[i] <- colors[team_name,]
}

#get current order of teams
current_team_order_SPI <- matchweek_tables_SPI[[length(matchweek_tables_SPI)]][["Name"]]
current_color_order_SPI <- rep(NA, teams_n)
for (i in 1:teams_n) {
  current_color_order_SPI[i] <- colors[current_team_order_SPI[i],]
}

#keep the order consistent
weekly_table_SPI$color <- factor(weekly_table_SPI$color, levels = current_color_order_SPI)




#create SPI plot
SPI_plot <- ggplot(data = weekly_table_SPI, aes(x = matchday, y = SPI)) +
  geom_line(aes(color = color), size = 1.5) +
  #scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order_SPI) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "SPI rating", title = "2021-2022 EPL SPI rating by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/SPI_rating_by_week.png", width = 1200, height = 700)
show(SPI_plot)
dev.off()



#create offensive SPI plot
Off_SPI_plot <- ggplot(data = weekly_table_SPI, aes(x = matchday, y = Off)) +
  geom_line(aes(color = color), size = 1.5) +
  #scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order_SPI) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Offensive SPI rating", title = "2021-2022 EPL Offensive SPI rating by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/SPI_rating_by_week_offense.png", width = 1200, height = 700)
show(Off_SPI_plot)
dev.off()


#create defensive SPI plot
Def_SPI_plot <- ggplot(data = weekly_table_SPI, aes(x = matchday, y = Def)) +
  geom_line(aes(color = color), size = 1.5) +
  #scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order_SPI) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Defensive SPI rating", title = "2021-2022 EPL Defensive SPI rating by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/SPI_rating_by_week_defense.png", width = 1200, height = 700)
show(Def_SPI_plot)
dev.off()
