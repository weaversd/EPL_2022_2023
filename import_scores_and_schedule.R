#import the schedule with dates and teams from fbref.com
schedule <- read_html("https://fbref.com/en/comps/9/11566/schedule/2022-2023-Premier-League-Scores-and-Fixtures")
#schedule_html <- html_nodes(schedule, 'th.right , .left:nth-child(3) a, .left:nth-child(9) a, .right a')
#schedule_html <- html_nodes(schedule, '.left:nth-child(3) a , .left:nth-child(7) a, .right a, th.right')
#schedule_html <- html_nodes(schedule, 'th.right , .left:nth-child(9) a, .right a, .left:nth-child(3) a')
schedule_html <- html_nodes(schedule, 'th.right , .left:nth-child(9) a, .right a, .left:nth-child(3) a, td.center')
schedule_text <- html_text(schedule_html)
#schedule_text <- stri_remove_empty(schedule_text)

determine_if_date <- function(string) {
  if (grepl("^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$", string)) {
    return(T)
  } else {
    return(F)
  }
}

date_list <- c()
matchday_list <- c()
home_list <- c()
away_list <- c()
score_list <- c()
for (i in 1:length(schedule_text)) {
  if (determine_if_date(schedule_text[i])) {
    date <- schedule_text[i]
    matchday <- schedule_text[i-1]
    home <- schedule_text[i+1]
    away <- schedule_text[i+3]
    score <- schedule_text[i+2]
    score <- str_replace_all(score, pattern = "[^0-9]", "~")
    date_list <- append(date_list, date)
    matchday_list <- append(matchday_list, matchday)
    home_list <- append(home_list, home)
    away_list <- append(away_list, away)
    score_list <- append(score_list, score)
  }
}

score_df <- data.frame(Matchday = matchday_list,
                       Date = date_list,
                       Home = home_list,
                       Away = away_list,
                       rawscore = score_list)

score_df$Date <- as.Date(score_df$Date)

#set the number of imported columns
#imported_columns_n <- 5

# #create a dataframe to hold the schedule
# schedule_df <- data.frame(matrix(ncol = imported_columns_n))
# 
# #take text string and parse into dataframe for the schedule
# for (i in 1:(length(schedule_text))) {
#   if (i%%imported_columns_n == 1) {
#     schedule_df[(i%/%imported_columns_n) + 1 ,1] <- schedule_text[i]
#   } else if (i%%imported_columns_n == 2) {
#     schedule_df[(i%/%imported_columns_n) + 1 ,2] <- schedule_text[i]
#   } else if (i%%imported_columns_n == 3){
#     schedule_df[(i%/%imported_columns_n) + 1 ,3] <- schedule_text[i]
#   } else if (i%%imported_columns_n == 0) {
#     schedule_df[(i%/%imported_columns_n), 4] <- schedule_text[i]
#   }
# }


#schedule_matrix <- matrix(schedule_text, ncol = imported_columns_n, byrow = T)
#schedule_df <- as.data.frame(schedule_matrix, stringsAsFactors = F)


#rename columns
#colnames(schedule_df) <- c("Matchday", "Date", "Home", "Away")
#schedule_df$Date <- as.Date(schedule_df$Date)

#create_scores_df
scores_df <- score_df

#add columns to score df
scores_df$H_score <- NA
scores_df$A_score <- NA
scores_df$Winner <- NA
scores_df$Loser <- NA
scores_df$Draw <- NA



#get scores
#score_import <- read_html("https://fbref.com/en/comps/9/schedule/Premier-League-schedule-and-Fixtures")
#score_import_html <- html_nodes(score_import, 'td.center')
#score_import_text <- html_text(score_import_html)
#score_import_text <- score_import_text[-(seq(11,417,11))]
#score_import_text <- score_import_text[-c(11,22,33,44,55,66,77,88,99,110,
#                                          121,132,143,154,165,176, 187,195,206,
#                                          217,228,232,243,254,265,276,287,
#                                          298,309,320,331,342,353,364,375,386,
#                                          397,408)]
#is.na(score_import_text) <- score_import_text == ""

#score_import_text <- str_replace_all(score_import_text, pattern = "[^0-9]", "~")

#save scores in column
#scores_df$rawscore <- score_import_text


#parse the text to get scores
####HERE IS THE ISSUE: The '-' symbol
for (i in 1:nrow(scores_df)) {
  if (!is.na(scores_df$rawscore[i])) {
    temp_strings <- str_split(scores_df$rawscore[i], "~")
    scores_df$H_score[i] <- as.numeric(temp_strings[[1]][1])
    scores_df$A_score[i] <- as.numeric(temp_strings[[1]][2])
  }
}


scores_df <- subset(scores_df, select= -(rawscore))

#populate wins and losses
scores_df <- populate_wins_losses(scores_df)

#get team list
team_list <- sort(unique(scores_df$Home))

#real time table
current_table <- create_table()

