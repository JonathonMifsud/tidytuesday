## TidyTuesday 05/08/19
## Bob Ross Paintings

library(tidyverse)
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
