### Tidy Tuesday
### AUG 23 2021


library(ggplot2)
library(dplyr)
library(tidyr)
library(igraph)
library(RColorBrewer)



lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

selected_id <- "3530"

lemurs %>%
  filter(sire_id == selected_id) %>%
  group_by(dlc_id, sex, taxon, dam_id, sire_id) %>%
  summarise() %>%
  pivot_longer(cols = c(dam_id, sire_id),
               names_to = "relation",
               values_to = "parent_id") %>%
  mutate(sex = ifelse(sex == "F", "Female", "Male"),
    parent_sex = ifelse(relation == "dam_id", "Female", "Male")) %>%
  select(parent_id, child_id = dlc_id, parent_sex, taxon, child_sex = sex) %>%
  ungroup() -> df_edge_list




df_edge_list %>%
  mutate(inbredt_child = ifelse(parent_id %in% intersect(df_edge_list$parent_id, df_edge_list$child_id),
                               1, 0)) %>%
  select(id = child_id,
         sex = child_sex,
         taxon,
         inbredt_child) %>%
  bind_rows(df_edge_list %>%
              mutate(parent_only = ifelse(parent_id %in% setdiff(df_edge_list$parent_id, df_edge_list$child_id),
                                          1, 0)) %>%
              select(id = parent_id,
                     sex = parent_sex,
                     taxon,
                     parent_only)) %>%
  mutate(inbredt_child = ifelse(is.na(inbredt_child), 0, inbredt_child),
         parent_only = ifelse(is.na(parent_only), 0, parent_only)) %>%
  group_by(id) %>%
  summarise(sex = first(sex),
            taxon = first(taxon),
            inbredt_child = first(inbredt_child),
            parent_only = first(parent_only)) %>%
  mutate(shape = ifelse(sex == "Female", "circle", "square"),
         role = case_when(
           id  == selected_id ~ "Ego",
           inbredt_child == 1 ~ "Inbred child",
           parent_only == 1 ~ "Parent only",
           TRUE ~ "Child"
         ),

         color = case_when(
           role  == "Ego" ~ "black",
           role == "Inbred child" ~ "red",
           role == "Parent only" ~ "darkgreen",
           role == "Child" ~ "orange"
         )) -> df_nodes



df_edge_list %>%
  select(parent_id, child_id) %>%
  graph_from_data_frame(vertices = df_nodes) -> g

png(file = "./20210823_lemurs/20210823_lemurs.png", width = 800, height = 600)

par(mar=c(2, 2, 5, 10))
set.seed(1)
plot(g,
     vertex.label = NA,
      layout = layout.fruchterman.reingold,
     vertex.frame.color = NA,
     vertex.shape = V(g)$shape,

     vertex.color = V(g)$color)


legend("right",
       horiz = FALSE,
       inset = c(-0.2, 0),
       legend = c("Ego",
                  "Male Child", "Female Child",
                  "Male Inbred Child", "Female Inbred Child",
                  "Parent only"),
       col = c("black", "orange", "orange", "red", "red", "darkgreen"),
       box.lty=0,
       pt.cex = 3,
       cex = 1.4,
       pch = c(15, 15, 19, 15, 19, 19),
       xpd = TRUE)

title("Tellus' partners and children (ID 3530)", cex.main = 3)


dev.off()
