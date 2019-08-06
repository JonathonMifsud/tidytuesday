## TidyTuesday 05/08/19
## Bob Ross Paintings

library(tidyverse)
library(ggsci)
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

## In total we have 31 seasons each with a number of episodes.
## I am thinking of combining all elements into a total count.
## With this I will look at whether the total count of elements
## changes across seasons. 


# to clean up the episode information
bob_ross <- bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer)

bobseason <- bob_ross %>%
  select(-title, -episode) %>% 
  group_by(season) %>% 
  summarise_all(list(totalsum = sum)) %>% 
  mutate(rowsum = rowSums(.[2:68])) %>% 
  select(season, rowsum)

bobepisode <- bob_ross %>%
  select(-title) %>% 
  group_by(episode) %>% 
  summarise_all(list(totalsum = sum)) %>% 
  mutate(rowsum = rowSums(.[3:68])) %>% 
  mutate(episode = as.factor(episode))

bobepiseason <- bob_ross %>% 
  select(-title) %>% 
  mutate(rowsum = rowSums(.[3:69])) %>% 
  select(season, episode, rowsum)

extrafont::loadfonts(device = "win")
ggsci
theme_set(theme_light(base_size = 15, base_family = "Poppins"))

g <- ggplot(bobepisode, aes(x = episode, y = rowsum, color = episode)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90), expand = c(0.005, 0.005)) +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid = element_blank())

g +
  geom_segment(aes(x = region, xend = region,
                   y = world_avg, yend = student_ratio_region),
               size = 0.8) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 5)