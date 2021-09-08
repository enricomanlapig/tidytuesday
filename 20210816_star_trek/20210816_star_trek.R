# Tidy Tuesday
# Aug 17, 2021


# Libraries

library(dplyr)
library(tidyr)
library(igraph)
library(RColorBrewer)


# Get the Data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


# Clean up char names

computer %>%
  mutate(char = case_when(
    grepl("com panel", char, ignore.case = TRUE) ~ "Computer",
    grepl("computer", char, ignore.case = TRUE) ~ "Computer",
    grepl("data", char, ignore.case = TRUE) ~ "Data",
    grepl("geordi", char, ignore.case = TRUE) ~ "Geordi",
    grepl("krag", char, ignore.case = TRUE) ~ "Krag",
    grepl("lwaxana", char, ignore.case = TRUE) ~ "Lwaxana",
    grepl("picard", char, ignore.case = TRUE) ~ "Picard",
    grepl("riker", char, ignore.case = TRUE) ~ "Riker",
    grepl("satelk", char, ignore.case = TRUE) ~ "Satelk",
    grepl("worf", char, ignore.case = TRUE) ~ "Worf",
    TRUE ~ char),
    char = gsub("(Cont'D)", "", char, fixed = TRUE),
    char = gsub("(O.S.)", "", char, fixed = TRUE)) %>%
  count(char, pri_type) %>%
  filter(char != "Computer",
         n >= 2) -> df


# Color palette for edges
palette <- brewer.pal(length(unique(df$pri_type)), "Dark2");


# Make the graph object

df %>%
  mutate(edge_color = as.character(factor(pri_type, labels = palette))) %>%
  right_join(df, by = "pri_type") %>%
  select(from = char.x,
         to = char.y,
         edge_color,
         n = n.x) %>%
  filter(from != to) %>%
  graph_from_data_frame(directed = FALSE) -> g


# Communities

ebc <- cluster_fast_greedy(simplify(g))
V(g)$community <- ebc$membership


# Draw some pics

png(file = "./20210816_star_trek/20210816_star_trek.jpeg", width = 800, height = 600)

par(bg = "black")

set.seed(1)

plot(g,
     layout=layout.kamada.kawai,
#     vertex.color = V(g)$community,
     vertex.shape = "sphere",
     vertex.frame.color = NA,
     edge.color = adjustcolor(E(g)$edge_color, alpha.f = 0.3),
     vertex.label.color = "white",
     vertex.label.family = "sans")

legend("right",
       horiz = FALSE,
       legend = unique(df$pri_type),
       text.col = unique(E(g)$edge_color),
       box.lty=0,
       bg = "black")

title("Humans sharing interaction types", cex.main=2, col.main="White")

dev.off()


