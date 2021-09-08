# Tidy Tuesday
# Aug 11, 2021


# Libraries

library(dplyr)
library(ggplot2)
library(ggbump)
library(ggthemes)

# Get the Data
chain_investment <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv'
  )



chain_investment %>%

  # Filter to basic, social, and digital categories

  filter(group_num %in% c(4, 17, 22)) %>%
  mutate(
    meta_cat = case_when(meta_cat == "Total basic infrastructure" ~ "Basic",
                         TRUE ~ meta_cat),
    category = case_when(
      meta_cat == "Digital" ~ "Digital",
      category == "Conservation and development" ~ "Conservation & \n Development",
      TRUE ~ category)) %>%

  # Make decades

  mutate(decade = cut(year, breaks = seq(1947, 2017, 10), include.lowest = TRUE, dig.lab = 4, ordered = TRUE)) %>%

  # Aggregate, compute proportions, and rank

  group_by(decade, meta_cat, category) %>%
  summarise(investment = sum(gross_inv_chain)) %>%

  group_by(decade) %>%
  mutate(prop = investment / sum(investment),
         rank = rank(-prop, ties = "random")) %>%


  # Labels

  ungroup() %>%
  mutate(
    start_label = ifelse(decade == first(decade), category, NA),
    end_label = ifelse(decade == last(decade), category, NA)) %>%

  # Picture time

  ggplot(aes(
    x = decade, y = rank, color = meta_cat, group = category)) +
  geom_bump(lwd = 2) +
  geom_text(
    aes(x = first(decade), label = start_label, color = meta_cat),
    hjust = 1, nudge_x = -0.1, show.legend =  FALSE) +
  geom_text(
    aes(x = last(decade), label = end_label, color = meta_cat),
    hjust = 0, nudge_x = 0.1, show.legend =  FALSE) +

  expand_limits(x = c(-0.5, 10)) +
  scale_y_reverse(breaks = c(1:8)) +
  theme_economist() +
  scale_color_economist()+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  labs(
    color = "",
    x = "Decade",
    y = "Rank",
    title = "Infrastructure categories ranked by share of total real gross investment")

ggsave("20210810_bea_investment/tidy_tuesday_2021_08_10.png")
