# Tidy Tuesday - Nuclear Explosions
# Jonathon Mifsud - 20/08/2019

library(tidyverse)

#Import Data
nuclear_explosions <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")


#Cleaning Country Names
nuclearbin <- nuclear_explosions %>%
  mutate(
    country = fct_recode(
      country,
      "China" = "CHINA",
      "France" = "FRANCE",
      "India" = "INDIA",
      "Pakistan" = "PAKIST",
      "United Kingdom" = "UK",
      "United States" = "USA",
      "Russia" = "USSR")) %>%
  mutate(year = cut(year, breaks = seq(1991))) %>% #breaking year into 2 categories 1945-1991 & 1991-1998
  mutate(year = as.character(year))

nuclearbin$year[!is.na(nuclearbin$year)] <- "Cold War (1945-1991)" #renaming bins
nuclearbin$year[is.na(nuclearbin$year)] <-
  "Post Cold War (1992-1998)"

nuclearbin <- nuclearbin %>%
  mutate(year = as.factor(year))


#Theme
map_theme <- 
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(size = 18),
    legend.title = element_text("Countries", size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "grey95"),
    strip.background = element_rect(
      color = "grey10",
      fill = "grey75",
      size = 1.5,
      linetype = "solid"
    )
  )

mycols <- #colors used for countries
  c("#e6ab02",
    "#a6761d",
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e")


#Plot
plot1 <- nuclearbin %>%
  ggplot(aes(longitude, latitude, color = country)) +
  borders(world) + #adding map
  geom_point(size = 2, alpha = 0.4) +
  scale_colour_manual(values = mycols) +
  facet_wrap(~ year, nrow = 2)

plot1 <- plot1 + coord_map("mollweide", #reshapping plot to accommodate coordinates better
                           xlim = c(-180, 180),
                           ylim = c(-60, 90)) +
  map_theme + #applying our theme
  labs(color = "Country",
       title = "Nuclear Explosions Worldwide (1945 - 1998)",
       caption = "Author: @jonathon_mifsud, Source: SIPRI") +
  guides(colour = guide_legend(override.aes = list(size = 10))) #changing guide size

#Saving Plot
ggsave(
  "nuclear_explosions.png",
  plot = plot1,
  width = 40,
  height = 30,
  units = "cm"
)