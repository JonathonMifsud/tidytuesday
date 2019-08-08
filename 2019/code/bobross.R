## TidyTuesday 05/08/19
## Bob Ross Paintings

library(tidyverse)
library(ggridges)
library(viridis)# for scale_fill_viridis in one of the extra plots
library(hrbrthemes) # for theme_ipsum in main plot

bob_ross <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv"
  )

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

bobepiseason <- bob_ross %>%
  select(-title) %>%
  mutate(rowsum = rowSums(.[3:69])) %>% #sum all elements across each row
  select(season, episode, rowsum) %>%
  mutate(episode = as.factor(episode))



# Final plot
plot <- bobepiseason %>%
  mutate(episode = fct_rev(episode)) %>%
  ggplot(aes(y = episode, x = rowsum, fill = episode)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 18,
    binwidth = 1,
    scale = 0.95
  ) +
  geom_text( # adding the numbers for each bin
    stat = "bin",
    aes(
      y = group + 0.95 * (..count.. / max(..count..)),
      label = ifelse(..count.. > 0, ..count.., "")
    ),
    vjust = 1,
    size = 3,
    color = "black",
    binwidth = 1
  ) +
  annotate( #annotation next to arrow
    "text",
    x = 13.7,
    y = 7.55,
    fontface = "bold",
    size = 3.5,
    label = "Across all seasons Episode 7 \n had 11 elements on 4 occasions"
  ) +
  theme_ridges(grid = FALSE) +
  theme(
    legend.position = "none",
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(hjust = 0.5, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8,
                                color = "#939184")
  )


# adding Labels
plot <- plot + labs(
  x = "Number of Elements in a Painting",
  y = "Episode Number",
  title = "Are the number of elements to each Bob Ross painting consistent across each episode?",
  subtitle = "Occurence across all 33 seasons",
  caption = "Author: @jonathon_mifsud, Source: FiveThirtyEight"
)

# arrows
arrows <-
  tibble(x1 = 12.3, #arrow coords
         x2 = 11.6,
         y1 = 7.7,
         y2 = 7.3) 

# adding arrows
p <- plot + geom_curve(
    data = arrows,
    inherit.aes = FALSE,
    aes(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2),
    arrow = arrow(length = unit(0.10, "inch")),
    size = 0.8,
    color = "gray20",
    curvature = 0.20
  )

# saving plot
ggsave(
  "bobross.png",
  plot = p,
  width = 40,
  height = 18,
  units = "cm"
)





## Extras
bobseason <- bob_ross %>%
  select(-title,-episode) %>%
  group_by(season) %>%
  summarise_all(list(totalsum = sum)) %>%
  mutate(rowsum = rowSums(.[2:68])) %>%
  select(season, rowsum)

bobepisode <- bob_ross %>%
  select(-title) %>%
  group_by(episode) %>%
  summarise_all(list(totalsum = sum)) %>%
  mutate(rowsum = rowSums(.[3:68])) %>%
  mutate(episode = as.factor(episode)) %>%
  select(episode, rowsum) %>%
  na.omit

# First draft of final plot
ggplot(bobepiseason, aes(x = rowsum, y = episode, fill = episode)) +
  geom_density_ridges(scale = 1,
                      jittered_points = TRUE,
                      alpha = 0.8) +
  theme_ridges() +
  theme(legend.position = "none")

# Second draft of final plot
ggplot(bobepiseason, aes(x = `rowsum`, y = `episode`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "rowsum", option = "F") +
  labs(title = 'Title') +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
