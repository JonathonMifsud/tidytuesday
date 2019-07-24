###########################################
##      TidyTuesday Bird Strikes         ##
##            Jonathon Mifsud            ##
###########################################

library(rstudioapi)
library(tidyverse)
library(magrittr)
library(lintr)
library(sf)
library(raster)
library(viridis)
library(cowplot)
library(rvest)
library(albersusa)

### Cleaning

# I wanted to try a spatial plot this week. The dataset provided would need some rearranging for this to work.
# The first thing I did was look for infomation on how busy each airport in the US is.
# From this I can gather a really rough estimate of the amount of flights and aggregate this acrosss each state.
# I then can compare this to the number of bird strike incidents.
# Airport data taken from tables found in https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States

airport_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States"
  )

# The tables came in two parts with slightly different headings so I extracted them, cleaned and join them together
# Used rvest to import the table and the chrome extension SelectorGadget to obtain the xpath's

ad1 <- airport_data %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()

ad1_clean <- ad1 %>%
  dplyr::select("State", "2017[3]", "2016[4]", "2015[5]", "2014[6]") %>%
  rename(
    "state" = "State",
    "2017" = "2017[3]",
    "2016" = "2016[4]",
    "2015" = "2015[5]",
    "2014" = "2014[6]"
  )

ad2 <- airport_data %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()

ad2_clean <- ad2 %>%
  dplyr::select(-"IATACode",-"2018",-"Airports (Medium Hubs)",-"City Served",-"Rank(2017)") %>%
  rename("state" = "State",
         "2015" = "2015[4]",
         "2014" = "2014[1]")

## A horriblly unclean way to convert these to numeric and remove commas but it was the only one I found to work of the top of my head
ad1_clean$`2017` <- as.numeric(gsub(",", "", ad1_clean$`2017`))
ad1_clean$`2016` <- as.numeric(gsub(",", "", ad1_clean$`2016`))
ad1_clean$`2015` <- as.numeric(gsub(",", "", ad1_clean$`2015`))
ad1_clean$`2014` <- as.numeric(gsub(",", "", ad1_clean$`2014`))

ad2_clean$`2017` <- as.numeric(gsub(",", "", ad2_clean$`2017`))
ad2_clean$`2016` <- as.numeric(gsub(",", "", ad2_clean$`2016`))
ad2_clean$`2015` <- as.numeric(gsub(",", "", ad2_clean$`2015`))
ad2_clean$`2014` <- as.numeric(gsub(",", "", ad2_clean$`2014`))

passangers <- rbind(ad1_clean, ad2_clean)

passangers <- passangers %>%
  mutate(mean_pass = rowMeans(dplyr::select(passangers, -state))) %>%
  dplyr::select(state, mean_pass) %>%
  group_by(state) %>%
  summarise(mean_pass = sum(mean_pass)) %>% #calculating an average passanger count across 2017:2014
  mutate(state = recode(state, "OH/KY" = "KY")) #for the purposes of the analysis it is easier to break these up


# Bird data
bird_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

bird_trim <- bird_impacts %>%
  dplyr::select("state", "incident_year") %>%
  filter(
    incident_year == 2017 |
      incident_year == 2016 |
      incident_year == 2015 |
      incident_year == 2014
  ) %>% #used the same years as the passanger dataset
  group_by(state) %>%
  summarise(incidents = sum(n()))


# Joining the two datasets
us_counties <-
  usa_sf(proj = c("longlat", "laea", "lcc", "eqdc", "aeqd"))
pas_bird <- merge(passangers, bird_trim, by = "state")

# as each state doesnt have spatial components I used https://geocode.localfocus.nl/ to obtain these as a csv. file "states"
states <-
  read.csv("state.csv", header = TRUE, stringsAsFactors = FALSE)
usa_strikes <- merge(pas_bird, states, by = "state")

#converting non sf to sf
usa_strikes_fips <- usa_strikes %>%
  st_as_sf(crs = 4326, coords = c("long", "lat"))

cont_usa_sightings <- st_join(us_counties, usa_strikes_fips)



### Plotting
# This section is heavily based upon Timo Grossenbacher great bivariate map tutorial: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(color = NA),
      panel.background = element_rect(color = NA),
      legend.background = element_rect(color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(
        size = 9,
        hjust = 0,
        color = "black"
      ),
      plot.title = element_text(
        size = 15,
        hjust = 0.5,
        color = "black"
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0.5,
        color = "black",
        margin = margin(
          b = -0.1,
          t = -0.1,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(t = 0.2,
                        b = 0,
                        unit = "cm"),
        color = "#939184"
      ),
      ...
    )
}

# create 3 buckets for incidents
quantiles_incidents <- cont_usa_sightings %>%
  na.omit() %>%
  pull(incidents) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for mean passangers
quantiles_mean_pass <- cont_usa_sightings %>%
  na.omit() %>%
  pull(mean_pass) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# As found on Timo Grossenbacher tutorial
# create color scale that encodes two variables
# red for incidents and blue for mean passangers
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949",
  # high incidents, high passangers
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1",
  # low incidents, high passangers
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A",
  # medium incidents, medium passangers
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E",
  # high incidents, low passangers
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low incidents, low passangers
) %>%
  gather("group", "fill")


# cut into groups defined above and join fill
cont_usa_sightings %<>%
  mutate(
    incidents_quantiles = cut(incidents,
                              breaks = quantiles_incidents,
                              include.lowest = TRUE),
    mean_pass_quantiles = cut(mean_pass,
                              breaks = quantiles_mean_pass,
                              include.lowest = TRUE),
    group = paste(
      as.numeric(incidents_quantiles),
      "-",
      as.numeric(mean_pass_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

### Blank map used for states that do not have data

us_all_counties <-
  usa_sf(proj = c("longlat", "laea", "lcc", "eqdc", "aeqd"))
states_all <-
  read.csv("all_states.csv",
           header = TRUE,
           stringsAsFactors = FALSE)

states_all_fips <- states_all %>%
  st_as_sf(crs = 4326, coords = c("long", "lat"))

all_usa <- st_join(us_all_counties, states_all_fips)

# Final plot starts here
map <- ggplot(
  data = cont_usa_sightings) +
  geom_sf(aes(fill = "gray88"), # states that have no data
          color = NA,
          size = 0.2,
          data = all_usa) +
  geom_sf(aes(fill = fill),
          # line for state borders
          color = "gray95",
          size = 0.2) +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = NULL,
    title = "More flights, more wildlife strikes?",
    subtitle = paste0("Average reported wildlife strikes by airplanes across 34 US states from 2014-2017"),
    caption = "Map: Author:"
  ) +
  theme_map()

bivariate_color_scale %<>%
  separate(group,
           into = c("incidents", "mean_pass"),
           sep = " - ") %>%
  mutate(gini = as.integer(incidents),
         mean = as.integer(mean_pass))

legend <- ggplot() +
  geom_tile(data = bivariate_color_scale,
            mapping = aes(x = incidents,
                          y = mean_pass,
                          fill = fill)) +
  scale_fill_identity() +
  labs(x = "Higher incidents",
       y = "Higher number of flights") + #technically passangers
  theme_map() +
  theme(axis.title = element_text(size = 8)) +
  coord_fixed()


ggdraw() +
  draw_plot(full_boarders, 0, 0, 1, 1) +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)

