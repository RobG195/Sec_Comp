library(rvest)
library(tidyverse)
library(gt)
library(gtExtras)

#Saves the url with the table, pulls the table labled in the code
url <- "https://www.cnbc.com/2024/12/19/college-sports-programs-valuations.html"
page <- read_html(url)
values <- page %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = T, header = TRUE)

#Saves the Top 10 and adds logos to the dataframe
values_10 <- head(values, 10)
values_10$logo <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/",
                         values_10$Program, ".png")

#Removes the dollar sign the B and the M and multiplies by 1000 for the billion
values <- values %>% mutate(value_millions = Valuation %>% 
                              gsub("\\$", "", .) %>% gsub("B", "e3", .) %>% 
                              gsub("M", "", .) %>% as.numeric()) #Makes Numeric

values <- values %>% mutate(revenue_millions = Revenue %>% 
                              gsub("\\$", "", .) %>% gsub("M", "", .) %>% as.numeric()) %>% 
  select(Rank, Program, Conference, value_millions, revenue_millions) #Selects the desired columns

#Pastes logos in dataframe
values_10$logo <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/",
                                values_10$Program, ".png")
values_10$logo <- values_10$logo %>% gsub("&", "", .)
  
values_10 %>% select(Rank, logo, Program, Conference, Revenue, Valuation) %>% 
  gt() %>% gt_img_rows(logo, img_source = "local") %>%
  cols_label(Rank = "", logo = "", Program = "School", Conference = "Conference", 
             Revenue = "Revenue", Valuation = "Valuation") %>% 
  cols_align(align = "center",columns = everything()) %>%
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>%
  tab_header(title = "Most Valuable Athletic Programs - 2024") %>% 
  gt_highlight_cols(Valuation, fill = "gold", alpha = 0.5) %>% 
  tab_style(style = cell_fill(color = "gray93"), locations = cells_body(columns = Conference,                         # Which column
      rows = grepl("SEC", Conference)))

table <- values %>% group_by(Conference) %>% summarize(n = n(), 
                                                       avg_val = round(mean(value_millions), digits = 1),
                                                       avg_rev = round(mean(revenue_millions), digits = 1),
                                                       avg_rank = round(mean(Rank), digits = 1)) %>% 
                                                         arrange(avg_rank)

table$logo <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/",
                         table$Conference, ".png")

table %>% select(logo, n, avg_val, avg_rev, avg_rank) %>% 
  gt() %>% gt_img_rows(logo, img_source = "local") %>%
  cols_label(logo = "", n = "Count", avg_val = "AVG Value (millions)", 
             avg_rev = "AVG Revenue (millions)", avg_rank = "AVG rank in Top 75") %>% 
  cols_align(align = "center",columns = everything()) %>%
  tab_options(heading.title.font.size = 32, source_notes.font.size = 12,
              column_labels.font.weight = "bold", column_labels.font.size = 18,
              heading.title.font.weight = "bold", heading.subtitle.font.size = 15)  %>%
  tab_header(title = "Top 75 Most Valuable Athleitc Programs by Conference - 2024") %>% 
  gt_highlight_cols(avg_rank, fill = "gold", alpha = 0.5) 

  
  
