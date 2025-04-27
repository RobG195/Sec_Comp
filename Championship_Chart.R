#### Creating Dataframe ####
#Adding libraries and allowing Roboto font to be used in graphs
library(tidyverse)
library(ggimage)
library(scales)
library(ggplot2)
library(showtext)
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

#Creating vectors of conference names, championships, graph colors
Conference <- c("SEC", "Big 10", "ACC", "PAC 12", "Big 12")
Wins <- c(16, 4, 4, 2, 2)
Colors <- c("darkgoldenrod1", "dodgerblue1", "royalblue", "black", "firebrick1")

#Creating a dataframe from these vectors and adding conference logos from locally saved filepath
df <- as.data.frame(cbind(Conference, Wins, Colors))
df$image_file <- paste0("/Users/robg/Documents/R_Projects/Sec_Comp/Logos/", df$Conference, ".png")
df$Wins <- as.numeric(df$Wins)

#### GG Plot ####
#GG Plot bar chart with a theme I have used in the past
ggplot(df, aes(x = reorder(Conference, Wins), y = Wins, fill = Conference)) + 
  geom_bar(stat = "identity", fill = df$Colors) + 
  theme(plot.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
        panel.background = element_rect(fill = "antiquewhite1", color = "antiquewhite1"),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, family = "Roboto", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, family = "Roboto"),
        axis.title = element_text(family = 'Roboto', face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 9, hjust = 1, family = "Roboto", 
                                    face = "bold"),
        panel.grid = element_line(color = "gray80"),
        axis.title.y = element_text(margin = margin(r = -0.25, unit = "in"))
  ) + 
  theme(legend_position = "none") + 
  coord_flip() + # Makes the bar chart horizontal instead of vertical
  geom_image(aes(image = image_file), asp = 2.5) + # Adds conference logos to graph
  labs(x = "", y = "Championships", title = "CFB National Championships", 
       subtitle = "(since 1998)") + 
  geom_label(aes(label = Wins), hjust = 3.5, color = "black", fill = "antiquewhite1",
             size = 4, label.padding = unit(0.3, "lines")) # Adds numbers within the bars
