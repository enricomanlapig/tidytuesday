library(dplyr)
library(ggplot2)
library(viridis)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')

ratings %>%
  filter(year >= 1990, year <= 2023) %>%
  mutate(period = cut(year, 
                      breaks = seq(min(ratings$year),
                                  max(ratings$year),
                                  by = 5))) %>%
  ggplot(aes(x = rank, y = average)) +
  geom_point(aes(color = period), 
             alpha = 0.3) +
  theme_minimal() + 
  scale_color_viridis(option = "B",
                      discrete = TRUE) +
  theme(legend.position = "top") +
  labs(title = "Average game ratings by period",
       y = "Average rating",
       x = "Game rank")

ggsave("20220124_board_games/20220124_board_games.png", 
       width = 6,
       height = 4,
       bg = "white")
