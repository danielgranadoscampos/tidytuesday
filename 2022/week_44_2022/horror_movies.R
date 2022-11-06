
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(showtext)
library(sysfonts)


# Fonts -------------------------------------------------------------------


font_add_google("Nosifer", "nosifer")
showtext_auto()

# Data --------------------------------------------------------------------

horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')


top_voted <- horror_movies|>
  mutate(release_year = year(release_date),
         release_month = month(release_date, label = TRUE),
         release_decade = factor(floor(release_year / 10) * 10)) |>
  filter(status == "Released") |>
  select(title, release_month, vote_average, vote_count, release_decade, popularity) |>
  arrange(desc(vote_count)) |>
  head(2000)
  

# Plot --------------------------------------------------------------------


box_plot <- top_voted |>
  ggplot(aes(x=release_decade, y=vote_average)) +
  geom_boxplot(outlier.size = NULL, color="#cd2109", fill="orange", alpha = 0.9) +
  geom_jitter(color="#e3fa3a", size=0.5, alpha=0.9) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(colour = "black", fill = "#050608"),
        plot.background = element_rect(colour = "black", fill = "#050608"),
        plot.title = element_text(color="#cd2109", size=24, family="nosifer"),
        text =  element_text(color="#1760a2", size=14, face="bold"),                      
        axis.title.x = element_text(color="#cd2109", size=14, face="bold"),
        axis.title.y = element_text(color="#cd2109", size=14, face="bold")) +
  labs(title= "Do we like the classics?",
       subtitle = "Movies released in older decades seem to have a higher average rating",
       x ="Decade released", 
       y = "Average vote")
box_plot




