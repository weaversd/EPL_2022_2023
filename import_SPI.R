#import SPI grades from fivethirtyeight
SPI <- read_html("https://projects.fivethirtyeight.com/soccer-predictions/premier-league/")
SPI_html <- html_nodes(SPI, '#forecast-table div')
SPI_text <- html_text(SPI_html)
SPI_text <- stri_remove_empty(SPI_text)
SPI_text <- str_remove_all(SPI_text, "[0-9]* pts")
SPI_text <- str_remove_all(SPI_text, "[0-9]* pt")

#convert to a dataframe and rename columns
SPI_matrix <- matrix(SPI_text, ncol = 5, byrow = T)
SPI_df <- as.data.frame(SPI_matrix, stringsAsFactors = F)
colnames(SPI_df) <- c("Original_name", "Name", "SPI", "Off", "Def")


#make adjustments to the team names so they match the import from the scores dataframe
for (i in 1:nrow(SPI_df)) {
  if (SPI_df$Original_name[i] == "Man. City") {
    SPI_df$Name[i] <- "Manchester City"
  }
  if (SPI_df$Original_name[i] == "Man. United") {
    SPI_df$Name[i] <- "Manchester Utd"
  }
  if (SPI_df$Original_name[i] == "Newcastle") {
    SPI_df$Name[i] <- "Newcastle Utd"
  }
  if (SPI_df$Original_name[i] == "Leicester") {
    SPI_df$Name[i] <- "Leicester City"
  }
  if (SPI_df$Original_name[i] == "Norwich") {
    SPI_df$Name[i] <- "Norwich City"
  }
}
