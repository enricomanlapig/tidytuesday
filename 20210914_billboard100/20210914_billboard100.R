library(stopwords)
library(tidytext)
library(tokenizers)
library(dplyr)
library(ggplot2)
library(gganimate)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

billboard %>%
  mutate(date = as.Date(billboard$week_id, format = "%m/%d/%Y")) %>%
  group_by(performer, song) %>%
  summarise(first_date = min(date, na.rm = TRUE)) %>%
  mutate(year = as.numeric(format(first_date, format = "%Y"))) %>%
  group_by(year) %>%
  unnest_tokens(word, song, token = "words") %>%
  filter(!(word %in% stopwords(source = "snowball")) & !is.na(year)) %>%
  count(year, word) %>%
  group_by(word) %>%
  arrange(year) %>%
  mutate(cuml_n = cumsum(n)) %>%
  group_by(year) %>%
  mutate(rank = rank(-cuml_n, ties.method = "random")) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  ggplot(aes(x = rank, y = cuml_n)) + 
  geom_col() + 
  geom_text(aes(x = rank, y = -10, label = word), hjust = 1) +
  coord_flip() + 
  scale_x_reverse() +
  theme_minimal() +
  transition_states(year, transition_length = 4, state_length = 1) + 
  enter_fade() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Most frequently occuring words in Top 100 song titles",
       subtitle = 'Year: {closest_state}',
       x = "",
       y = "Cumulative count") -> g

animate(g, fps = 30, duration = 30, width = 800, height = 600)

anim_save("20210914_billboard100/20210914_billboard100.mp4")
