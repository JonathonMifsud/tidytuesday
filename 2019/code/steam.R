## TidyTuesday 30/7/19
## Sadly incomplete as of yet

## Steam Game data

library(tidyverse)
library(tidytext)

# load in data
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")


title_words <- video_games %>%
  filter(!str_detect(game, "[:digit:]")) %>%
  select(game, owners) %>% 
  unnest_tokens(word, game) %>%
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE)
  
  