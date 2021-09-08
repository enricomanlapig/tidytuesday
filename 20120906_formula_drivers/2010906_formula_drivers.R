library(dplyr)
library(ggplot2)
library(ggnetwork)

### Get data

tuesdata <- tidytuesdayR::tt_load('2021-09-07')



### ggnetwork

tuesdata$results %>%
  left_join(tuesdata$races, by = "raceId") %>%
  left_join(tuesdata$circuits, by = "circuitId") %>%
  mutate(teamId = paste("team_", constructorId, sep = "")) %>%
  select(driverId, teamId, raceId, year) -> df

df %>%
  full_join(df, by = c("teamId", "raceId", "year"), suffix = c("_from", "_to")) %>%
  mutate(from = ifelse(driverId_from < driverId_to, driverId_from, driverId_to),
         to = ifelse(driverId_from < driverId_to, driverId_to, driverId_from)) %>%
  select(from, to, raceId, year, teamId) %>%
  filter(from != to) %>%
  unique() %>%
  mutate(decade = cut(year, breaks = seq(min(df$year), max(df$year), 10), include.lowest = FALSE, dig.lab = 4)) %>%
  filter(!is.na(decade)) %>%
  graph_from_data_frame(directed = FALSE) %>%
  ggnetwork(by = "decade") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + geom_edges(aes(color = decade)) + geom_nodes() + 
  theme_blank() + 
  labs(title = "Race team networks", color = "")+
  theme(plot.background = element_rect(fill = "white"))

  
ggsave("20120906_formula_drivers/20210906_formula_drivers.png")

  

### gganimate 

library(gganimate)

tuesdata$driver_standings %>%
  filter(!(positionText %in% c("D", "E", "F", "N", "R", "W"))) %>%
  
  left_join(
    tuesdata$lap_times,
    by = c("driverId", "raceId"),
    suffix = c("_final", "_lap")) %>%
  
  group_by(raceId, driverId) %>%
  mutate(
    last_position_lap = lag(position_lap, order_by = lap),
    position_changed = ifelse(position_lap == last_position_lap, 0, 1)) %>%
  
  group_by(raceId) %>%
  summarise(
    avg_position_changes = mean(position_changed, na.rm = TRUE),
    avg_secs = mean(milliseconds, na.rm = TRUE) / 1000) %>%
  
  filter(avg_position_changes > 0) %>%
  
  left_join(tuesdata$races, by = "raceId") %>%
  
  left_join(tuesdata$circuits,
            by = "circuitId",
            suffix = c("_race", "_circuit")) %>%
  
  group_by(name_race) %>%
  mutate(num_races = n()) %>%
  filter(num_races >= 20) %>%
  ggplot(aes(x = avg_secs, y = avg_position_changes, color = name_race, label = name_race)) +
  geom_point(show.legend = FALSE,
             size = 4) +
  geom_text(show.legend = FALSE,
            check_overlap = TRUE,
            hjust = -0.2) +
  theme_minimal() +
  transition_time(year) +
  labs(title = "Avg. number of position changes per racer by Avg. lap time",
       subtitle = "Year: {frame_time}",
       x = "Avg. lap time (seconds)",
       y = "Avg. number of position changes")


anim_save("20120906_formula_drivers/20210906_formula_drivers.gif")

