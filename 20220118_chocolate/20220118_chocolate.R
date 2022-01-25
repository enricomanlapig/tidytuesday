library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(viridis)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

my_chars <- gsub(",", " ", df$most_memorable_characteristics)
my_chars <- gsub("  ", " ", my_chars)
my_chars <- str_split(my_chars,
                      pattern = " ",
                      simplify = TRUE)
my_chars <- trimws(my_chars)

cbind(df, my_chars) %>%
  pivot_longer(cols = as.character(c(1:ncol(my_chars))),
               values_to = "my_chars") %>%
  filter(!is.na(my_chars),
         my_chars != "") %>%
  group_by(my_chars) %>%
  summarise(avg_rating = mean(rating),
            num_chars = n()) %>%
  mutate(rating_order = rank(-avg_rating,
                             ties.method = "random"),
         num_chars_order = rank(-num_chars,
                                ties.method = "random")) %>%
  filter(num_chars_order <= 20) %>%
  ggplot(aes(x = reorder(my_chars, num_chars), 
             y = num_chars)) + 
  geom_col(aes(fill = avg_rating)) +
  coord_flip() +
  labs(title = "Top 20 most cited memorable characteristics",
       subtitle = "And average rating when cited",
       x = "Characteristic",
       y = "Number of times characteristic was cited",
       fill = "Average rating") +
  theme_minimal() +
  scale_fill_viridis()

ggsave("20220118_chocolate/20220118_chocolate.png",
       width = 5,
       height = 4,
       bg = "white")  

