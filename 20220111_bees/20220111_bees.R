library(dplyr)
library(ggplot2)
library(gganimate)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

df <- left_join(stressor, colony, by = c("year", "months", "state"))

months_levels <- c("January-March",
                  "April-June",
                  "July-September",
                  "October-December")

df %>%
  filter(year != "5/",
         months != "2019") %>%
  mutate(year = as.numeric(year),
         quarter = case_when(
           months == months_levels[1] ~ 1,
           months == months_levels[2] ~ 2,
           months == months_levels[3] ~ 3,
           months == months_levels[4] ~ 4),
         period = as.integer((year - min(year)) * 4 + quarter ),
         stress_pct = ifelse(is.na(stress_pct), NA, stress_pct)) %>%
  ggplot(aes(x = stress_pct,
             y = colony_lost_pct)) + 
  geom_point(aes(color = stressor), alpha = 0.8) +
  facet_wrap(~stressor) +
  theme_minimal() +
  labs(
    title = "Which stressors lead to colony loss?",
    y = "% of colonies lost in state",
    x = "% of colonies impacted by stressor"
  ) +
  theme(
    legend.position = "none"
  )-> my_graph

my_graph
ggsave("20220111_bees/bees.png", bg = "white")

my_graph + transition_states(period) +
  labs(subtitle = "Period: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE) -> my_animation

anim_save("20220111_bees/bees.gif", height = 800, width =800, res = 150)
