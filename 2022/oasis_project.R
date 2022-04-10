library(tidyverse)
library(stringr)



news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

news_data <- news_orgs %>% 
  mutate(year_founded = as.numeric(year_founded)) %>% 
  separate_rows(products, sep = ",") %>% 
  group_by(year_founded, products) %>% 
  summarise(n = n()) %>% 
  mutate(products = case_when(str_starts(products, " ") 
                              ~ substring(products, 2))) %>% 
  drop_na(products) 


p <- ggplot(news_data, aes(x=year_founded, group=products, fill=products)) +
  geom_density(adjust=1.5, position="fill") +
  xlab("Year founded") + 
  ylab("Density") +
  labs(fill = "Product",
       title = "How news products have changed based on the year a publication is founded",
       caption = "Data:Project Oasis") +
  theme_bw() +
  scale_fill_brewer(palette = "Set3")


ggsave("oasis_project.png", plot = p, width = 350, height = 300, units = "mm", dpi = "retina")

