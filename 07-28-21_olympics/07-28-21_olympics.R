


# Libraries

library(dplyr)
library(tidyr)
library(ggplot2)

# Get the Data

olympics <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv'
  )


olympics %>%

  distinct(season, year, sport, event, sex) %>%
  count(season, year, sex, name = "count") %>%
  pivot_wider(names_from = sex,
              values_from = count,
              values_fill = 0) %>%
  mutate(diff = M - F) %>%

  ggplot(aes(x = year)) +
  geom_linerange(aes(ymin = F,
                     ymax = M),
                 alpha = 0.3,
                 lwd = 1) +
  geom_point(aes(y = M, colour = "M"), size = 3) +
  geom_point(aes(y = F, colour = "F"), size = 3) +

  facet_wrap( ~ season) +

  labs(
    title = "Number of Olympic events by sex by year",
    x = "Year",
    y = "Number of events",
    color = "Sex") +

  scale_colour_manual(name = "Sex",
                      values = c(M = "seagreen4",
                                 F = "purple4")) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(colour = "black", fill = "white"),
    strip.background = element_rect(colour = "black", fill = "white")
  )

ggsave("tidy_tuesday_2021_07_28.png")
