library(tidyverse)
library(purrr)
library(magick)
library(viridis)


# Download data -----------------------------------------------------------


fuel_gdp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')

indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')

#Group by continent
fuel_list_continent <- fuel_gdp %>% 
  drop_na(Continent) %>% 
  group_by(Entity,Continent) %>% 
  summarize() 

#Complete continents in fuel data
fuel_data <- fuel_list_continent  %>% 
  full_join(fuel_gdp, by = "Entity") %>% 
  mutate_at(vars(matches("Continent")), funs(replace(., . == "", NA))) %>% 
  select(-Continent.y) %>% 
  rename(Continent = Continent.x) %>% 
  rename(GDP = "GDP per capita, PPP (constant 2017 international $)") %>% 
  rename(fuel = "Access to clean fuels and technologies for cooking (% of population)") %>%
  rename(population = "Population (historical estimates)") %>% 
  filter(Year > 1970) %>% 
  mutate(Continent = ifelse(Continent == "North America" | Continent == "South America", "Americas", Continent)) %>%
  drop_na()

#Complete continents in pollution data
indoor_pollution_data <- fuel_list_continent  %>% 
  full_join(indoor_pollution, by = "Entity") %>% 
  mutate_at(vars(matches("Continent")), funs(replace(., . == "", NA))) %>% 
  filter(Year > 1970) %>% 
  mutate(Continent = ifelse(Continent == "North America" | Continent == "South America", "Americas", Continent)) %>% 
  rename( deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`) %>% 
  drop_na()



# Complete data -----------------------------------------------------------


indoor_data <- inner_join(indoor_pollution_data, fuel_data)



# plot --------------------------------------------------------------------


p_points <- indoor_data %>%
  ggplot(aes(x = fuel, y = deaths, colour = Continent)) +
  geom_point(alpha = 0.4, size = 3, show.legend = FALSE) +
  scale_color_viridis(discrete= TRUE, option = "D") +
  facet_wrap(~Continent, nrow = 1) +
  ylab("Deaths caused by household air pollution risk (%)")+
  xlab("Population with access to clean fuels and technology (%)")+
  theme_minimal() +
  
  theme(panel.spacing = unit(2, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(1,1,1,1,"cm"),
        plot.title = element_text(size = 16, hjust = 0, vjust = 1, 
                                  face = "bold", family="sans", lineheight = 0.7),
        plot.subtitle = element_text(size=14),
        plot.background = element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title = element_text(size=14)) +
  
  labs(title = "Deaths from indoor air pollution decrease as populations gain access to clean fuels for cooking.",
       subtitle = "Visualization across continents")



# Saving plot -------------------------------------------------------------

ggsave("r_indoor_pollution.png", plot=p_points, width = 420, height = 250, units = "mm", dpi= "retina")

  
  
  
  
  
  
  


