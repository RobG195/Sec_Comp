library(tidyverse)
library(gt)
library(gtExtras)
Games <- read.csv("CFB_Games.csv")

#### Data ####
#I could have done this a lot faster and with a lot more code. I wanted to see how to do it
#using a loop to practice that skill
conferences <- c("SEC", "Big Ten", "Big 12", "ACC")

conference_stats <- data.frame(
  Conference = character(),
  WinPct = numeric(),
  OpponentElo = numeric(),
  TotalGames = integer(),
  Wins = integer(),
  Losses = integer(),
  stringsAsFactors = FALSE
)

for (conference in conferences) {
  conference_games <- Games %>%
    filter((HomeConference == conference | AwayConference == conference) &
             !(HomeConference == conference & AwayConference == conference)) %>%
    filter(HomeConference %in% c("SEC", "Big Ten", "Big 12", "ACC")) %>%
    filter(AwayConference %in% c("SEC", "Big Ten", "Big 12", "ACC")) %>%
    mutate(win = ((HomeConference == conference & HomePoints > AwayPoints) |
                    (AwayConference == conference & AwayPoints > HomePoints)))
  
  total_games <- nrow(conference_games)
  total_wins <- sum(conference_games$win)
  total_losses <- total_games - total_wins
  win_pct <- round(mean(conference_games$win) * 100, digits = 1)
  
  non_conf_home <- conference_games %>%
    filter(HomeConference != conference) %>%
    pull(HomePregameElo)
  
  non_conf_away <- conference_games %>%
    filter(AwayConference != conference) %>%
    pull(AwayPregameElo)
  
  avg_opponent_elo <- round(mean(c(non_conf_home, non_conf_away)), digits = 1)
  
  conference_stats <- rbind(conference_stats, data.frame(
    Conference = conference,
    WinPct = win_pct,
    OpponentElo = avg_opponent_elo,
    TotalGames = total_games,
    Wins = total_wins,
    Losses = total_losses
  ))
}

logo_conferences <- c("SEC", "Big 10", "Big 12", "ACC")
conference_stats$logo <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/",
                                logo_conferences, ".png")

#### Table ####
conference_stats %>% select(logo, WinPct, OpponentElo, TotalGames, Wins, Losses) %>% 
  gt() %>% gt_img_rows(logo, img_source = "local") %>% 
  cols_label(logo = "", WinPct = "Winning Percentage*", OpponentElo = "Opponent Elo",
             TotalGames = "Total Games", Wins = "Wins", Losses = "Losses") %>% 
  cols_align(align = "center",columns = everything()) %>% 
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>% 
  tab_header(title = "CFB Non-Conference Performance 2024") %>% 
  tab_style(style = cell_fill(color = "grey92"),
            locations = cells_body(rows = seq(2, 4, 2))) %>% 
  gt_highlight_cols(WinPct, fill = "gold", alpha = 0.5) %>% 
  tab_footnote(
    footnote = html("*against other power 4 conference teams in the regular season"))
