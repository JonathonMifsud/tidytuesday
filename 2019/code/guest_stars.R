# Tidytuesday 27/08/19
# Simpsons Guest Stars
# Jonathon Mifsud

library(tidyverse)
library(ggridges)
library(showtext)
library(ggpomological)
font_add_google(name = "Caveat", family = "Caveat")

# load dataset
simpsons <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv"
  )

simpsons_clean <- simpsons %>%
  filter(season != "Movie") %>% #removing movie
  separate(number, c("episode", NA)) #seperating episode number from season

guest <- simpsons_clean %>%
  group_by(guest_star) %>%
  filter(n() >= 18) %>% #filtering out the 6 most common guest stars
  mutate(season = as.numeric(season))

mycols <- #ridge colors
  c("#828585",
    "#c03728",
    "#919c4c",
    "#fd8f24",
    "#f5c04a",
    "#e68c7c")

mycolsdarker <- #point colours
  c("#5e6060",
    "#8b281d",
    "#697137",
    "#d06701",
    "#dc9b0c",
    "#c03c24")

mytheme <- theme(
  text = element_text(family = "Caveat"), # handwritten font
  plot.title = element_text(size = 32),
  plot.subtitle = element_text(size = 24),
  axis.title.x = element_text(size = 32, hjust = 0.5),
  axis.title.y = element_text(size = 32, hjust = 0.5),
  axis.text = element_text(size = 24)
)

# Main plot
showtext_auto() #enables the showtext font 
plot2 <- guest %>%
  group_by(season, guest_star) %>%
  summarise(apps = n()) %>%
  ggplot(aes(y = guest_star, x = season, fill = guest_star)) +
  geom_density_ridges(
    aes(
      point_color = guest_star,
      point_fill = guest_star,
      guide = "none"
    ),
    alpha = 0.8,
    scale = 2,
    size = 0.25,
    jittered_points = TRUE,
    point_size = 1
  ) +
  scale_fill_manual(values = mycols, guide = "none") +
  ggridges::scale_discrete_manual(aesthetics = "point_color",
                                  values = mycolsdarker,
                                  guide = "none") +
  ggridges::scale_discrete_manual(aesthetics = "point_fill",
                                  values = mycolsdarker,
                                  guide = "none") +
  scale_y_discrete(expand = expand_scale(add = c(0.3, 2.5))) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                     limits = c(0, 30)) +
  theme_ridges(grid = FALSE, center = TRUE)

pomo_plot <- plot2 +  theme_pomological(base_size = 20,
                                    base_theme = ggridges::theme_ridges())

pomo_plot <- pomo_plot + mytheme
pomo_plot <- pomo_plot + labs(
  x = "Season",
  y = "Guest Star",
  title = "Guest appearances across all Simpsons episodes",
  subtitle = "6 most common guest actors",
  caption = "Author: @jonathon_mifsud, Source: Wikipedia"
)
paint_pomological(pomo_plot, res = 110) %>%
  magick::image_write("simpsons_guest_stars.png")


### Extra

baseplot <- guest %>% #base plot
  group_by(season, guest_star) %>%
  summarise(apps = n()) %>%
  ggplot(aes(y = guest_star, x = season, fill = guest_star)) +
  geom_density_ridges(
    alpha = 0.8,
    scale = 2,
    jittered_points = TRUE,
    point_size = 0.85
  ) +
  scale_fill_manual(values = mycols, guide = "none") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                     limits = c(0, 30)) +
  theme_ridges(grid = FALSE, center = TRUE)


