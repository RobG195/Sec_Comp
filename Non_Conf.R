library(tidyverse)
library(gt)
library(gtExtras)
Games <- read.csv("CFB_Games.csv")

#### Data ####
conferences <- c("SEC", "Big Ten", "Big 12", "ACC")

#Creating an empty dataframe to store values 
conference_stats <- data.frame(
  Conference = character(),
  WinPct = numeric(),
  OpponentElo = numeric(),
  TotalGames = integer(),
  Wins = integer(),
  Losses = integer(),
  stringsAsFactors = FALSE
)

#Loop
for (conference in conferences) {
  conference_games <- Games %>%
    #Selects games containing teams from the current conference in the loop, but not intraconference games
    filter((HomeConference == conference | AwayConference == conference) & 
             !(HomeConference == conference & AwayConference == conference)) %>%
 #Filters for only games between power 4 conference teams   
    filter(HomeConference %in% c("SEC", "Big Ten", "Big 12", "ACC")) %>%
    filter(AwayConference %in% c("SEC", "Big Ten", "Big 12", "ACC")) %>%
    #Creates a binary column which is a 1 when the conference wins a game, and 0 when they lose
    mutate(win = ((HomeConference == conference & HomePoints > AwayPoints) |
                    (AwayConference == conference & AwayPoints > HomePoints)))
  
  #simple operations to calculate conference performance against other power conferences
  total_games <- nrow(conference_games)
  total_wins <- sum(conference_games$win)
  total_losses <- total_games - total_wins
  win_pct <- round(mean(conference_games$win) * 100, digits = 1)
  
  #Pulls opponent elo when the away team is the opponent
  non_conf_home <- conference_games %>%
    filter(HomeConference != conference) %>%
    pull(HomePregameElo)
  
  #Pulls opponent elo when the home team is the opponent
  non_conf_away <- conference_games %>%
    filter(AwayConference != conference) %>%
    pull(AwayPregameElo)
  
  #Finds the average opponent elo combining the two vectors
  avg_opponent_elo <- round(mean(c(non_conf_home, non_conf_away)), digits = 1)
  
  #Inputs the gathered statistics into the previously created dataframe
  conference_stats <- rbind(conference_stats, data.frame(
    Conference = conference,
    WinPct = win_pct,
    OpponentElo = avg_opponent_elo,
    TotalGames = total_games,
    Wins = total_wins,
    Losses = total_losses
  ))
}

#Adds the logo filepath to the dataframe 
logo_conferences <- c("SEC", "Big 10", "Big 12", "ACC")
conference_stats$logo <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/",
                                logo_conferences, ".png")

#### Table ####
conference_stats %>% select(logo, WinPct, OpponentElo, TotalGames, Wins, Losses) %>% #Selects desired columns
  gt() %>% gt_img_rows(logo, img_source = "local") %>% #Inputs images
  cols_label(logo = "", WinPct = "Winning Percentage*", OpponentElo = "Opponent Elo", 
             TotalGames = "Total Games", Wins = "Wins", Losses = "Losses") %>% #Names Columns
  cols_align(align = "center",columns = everything()) %>% #Centers Columns
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>% #formatting
  tab_header(title = "CFB Non-Conference Performance 2024") %>% #Title
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 4, 2))) %>% #Makes certain cells gray
  gt_highlight_cols(WinPct, fill = "gold", alpha = 0.5) %>% #Makes gold column
  tab_footnote(
    footnote = html("*against other power 4 conference teams in the regular season")) #Footnote
