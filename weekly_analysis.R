#import each matchweek table
matchweek_tables <- list()
matchweek_files <- list.files(path = "current_table_output/")

#save each to a different index in the list
for (i in 1:length(matchweek_files)) {
  matchweek_tables[[i]]<- read.csv(paste0("current_table_output/", matchweek_files[i]))
  matchday_str <- as.numeric(str_extract(matchweek_files[i], "\\d+")) ##multiple digits
  matchweek_tables[[i]]$matchday <- matchday_str
}

#combine into one df
weekly_table <- bind_rows(matchweek_tables)


#create a color dictionary

color_list <- c("#F7B6D0", "#670e36", "#F6403B", "#F4CF14", "#A0D5CB",
                "#034694", "#a7a5a6", "#87BDA0", "#000000", "#D7E400", "#003A83",
                "#009782", "#97c1e7", "#DA291C", "#523a28", "#AA1617",
                "#525252", "#132257", "#7f0000", "#fdb913")
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
weekly_table$color <- NA
for (i in 1:nrow(weekly_table)) {
  weekly_table$color[i] <- colors[weekly_table$Team[i],]
}

#get current order of teams
current_team_order <- current_table$Team
current_color_order <- rep(NA, teams_n)
for (i in 1:teams_n) {
  current_color_order[i] <- colors[current_team_order[i],]
}

#keep the order consistent
weekly_table$color <- factor(weekly_table$color, levels = current_color_order)

#change matchday to a numeric
weekly_table$matchday <- as.numeric(weekly_table$matchday)

#make the position plot
position_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Pos)) +
  annotate(geom = "rect", xmin = 1, xmax = max(weekly_table$matchday),
           ymin = 4.5, ymax = 1.5, fill = "palegreen", alpha = 0.35) +
  annotate(geom = "rect", xmin = 1, xmax = max(weekly_table$matchday),
           ymin = 20.5, ymax = 17.5, fill = "red", alpha = 0.1) +
  annotate(geom = "rect", xmin = 1, xmax = max(weekly_table$matchday),
           ymin = 1.5, ymax = 0.5, fill = "green", alpha = 0.3) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_reverse(breaks = 1:20) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20),
        legend.key.height = unit(0.57, "cm")) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Position", title = "2022-2023 EPL table position by week") +
  geom_point(aes(fill = color), size = 2, shape = 21, stroke = 0.5) +
  scale_fill_identity(guide = "legend",
                      labels = current_team_order)

#save as png

(position_plot)
ggsave("weekly_analysis/position_by_week.png", device = "png")


#keep the order consistent alphebetically for the remaining plots
#weekly_table$color <- factor(weekly_table$color, levels = color_list)

#make a percent UCL plot
UCL_plot <- ggplot(data = weekly_table, aes(x = matchday, y = UCL_pct)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Chance to make UCL (%)", title = "2022-2023 EPL chance to make UCL by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity(guide = "legend",
                      labels = current_team_order)

(UCL_plot)
ggsave("weekly_analysis/UCL_chance_by_week.png")

#make a percent relegation plot
relegation_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Relegated_pct)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Chance of relegation (%)", title = "2022-2023 relegation chance by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity(guide = "legend",
                      labels = current_team_order)

(relegation_plot)
ggsave("weekly_analysis/relegation_chance_by_week.png")

#make a percent title plot
title_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Prem_Title_pct)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Chance of winning the EPL (%)", title = "2022-2023 EPL title chance by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity(guide = "legend",
                      labels = current_team_order)


(title_plot)
ggsave("weekly_analysis/title_chance_by_week.png")

#make a points plot
points_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Pts)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Points", title = "2022-2023 EPL points by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity(guide = "legend",
                      labels = current_team_order)

(points_plot)
ggsave("weekly_analysis/points_by_week.png")

#make a GD plot
GD_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GD)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Goal Differential", title = "2022-2023 EPL goal differential by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

(GD_plot)
ggsave("weekly_analysis/goal_differential_by_week.png")

#make a GF plot
GF_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GF)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(limits = c(0,NA), breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Goals scored", title = "2022-2023 EPL goals scored by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

(GF_plot)
ggsave("weekly_analysis/goals_scored_by_week.png")

#make a GA plot
GA_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GA)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Week", y = "Goals against", title = "2022-2023 EPL goals against by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

(GA_plot)
ggsave("weekly_analysis/goals_against_by_week.png")

#run the by team weekly analysis
source("team_weekly_analysis.R")
source("create_facet_grid_team_pdf.R")



