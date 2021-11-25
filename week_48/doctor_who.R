library(datardis)
library(tidyverse)
library(ggplot2)
library(forcats)
library(showtext)
library(ggtext)
library(cowplot)
library(magick)
library(lubridate)


# data ------------------------------------------------------------------------

?episodes


holidays <- episodes %>% 
  group_by(year(first_aired))%>%
  mutate(avg_year_rating = mean(rating, na.rm = TRUE))%>% #Average rating per year
  filter(month(first_aired) == 12 & day(first_aired) == 25 )%>% #Christmas episodes
  mutate(episode_title = fct_reorder(episode_title, first_aired)) %>% #Order by date aired
  mutate(episode_year = paste(episode_title, " (", year(first_aired), ")")) %>% #  "episode (year)" variable
  mutate(episode_year = fct_reorder(episode_year, first_aired)) # order by date aired


# Fonts -----------------------------------------------------------------------

font_add(family="bold", "Doctor-Who.ttf")
font_add(family="regular", "torchwood_font_by_sostopher.ttf")
showtext_auto()



# The plot -------------------------------------------------------------------

p <- ggplot(holidays) +
  geom_segment( aes(x= episode_year, xend= episode_year, 
                    y= avg_year_rating, yend= rating), 
                size = 1, color="#56615c") +
  
  geom_point( aes(x= episode_year, y= avg_year_rating), 
              color= "#9d4b55", size=4 ) +
  
  geom_point( aes(x= episode_year, y= rating), 
              color= "#6a89ab", size=4 ) +
  
  labs(title = "Doctor Who: Christmas Specials", x="", y="Rating")+
  
  coord_flip()+
  
  theme(
    #titles
    plot.title = element_text(family="bold", size=30, color = "#213b54"),
    plot.title.position = "plot",
    #axis
    axis.title.x  = element_text(family = "regular", size =14, color = "black"),
    axis.text.y = element_text(family="regular", size=13, color="black"),
    axis.text.x = element_text(family="regular", size =13, color = "black"),
    axis.ticks.x = element_line(colour="black"),
    axis.ticks.y = element_blank(),
    #plot
    plot.background = element_rect(fill =  alpha("white", 0.6), 
                                   colour =  alpha("white", 0.5)),
    #panel
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", 
                                    colour =  "transparent"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
p

## Subtitle as in -------------------------------------------------------------
#https://www.r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html


p_fin <- p +   
  
  
  labs(subtitle = "<span style = 'color: #6a89ab;'>**Christmas special rating**
       </span>and <span style = 'color: #9d4b55;'>**average rating of episodes
       aired that year**</span> from 2005 to 2017<br>")+
  
  theme(plot.subtitle = element_markdown(size = 16, hjust = .05, 
                                         family = "regular"))
p_fin

## Backgroung image as in -----------------------------------------------------
#  https://github.com/nrennie/tidytuesday/blob/main/2021/23-11-2021/23112021.R 


img <- image_read("doctor_who_wallpaper.jpg")

q <- ggdraw() + 
  draw_image(img)  +
  draw_plot(p_fin, x=0.03, y=0, width = 0.95)
q