### Tidy Tuesday
### AUG 30 2021


library(ggplot2)
library(dplyr)



bird_baths <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv'
  )

bird_baths %>%

  filter(complete.cases(.)) %>%

  group_by(survey_year, urban_rural, bioregions) %>%
  summarise(num_species = sum(bird_count)) %>%

  group_by(urban_rural, bioregions) %>%
  filter(n() != 1) %>% # Only consider bioregions with 2014 data

  ungroup() %>%

  ggplot(aes(x = survey_year,
             y = num_species)) +
  geom_point(aes(color = urban_rural)) +
  geom_line(aes(color = urban_rural)) +

  facet_wrap(vars(bioregions),
             nrow = 1,
             labeller = label_wrap_gen(width = 10,
                                       multi_line = TRUE)) +

  theme_minimal() +

  scale_x_continuous(breaks = c(
    min(bird_baths$survey_year, na.rm = TRUE),
    max(bird_baths$survey_year, na.rm = TRUE)
  )) +

  theme(
    legend.position = "top",
    panel.spacing = unit(1, units = "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  ) +

  labs(
    x = "Survey year",
    y = "Number of species observed",
    color = "Area classification",
    title = "Variety of species visiting bird baths by bioregion and area"
  )

ggsave("20210830_birdbaths/20210830_birdbaths.png",
       width = 7, height = 5)
