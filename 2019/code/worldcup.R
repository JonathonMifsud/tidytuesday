###########################################
## TidyTuesday Women's World Cup Results ##
##            Jonathon Mifsud            ##
###########################################

# Reading data
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

wwc_outcomes <- left_join(wwc_outcomes, codes, by = "team")

library(tidyverse)


# The first plot that I want to try out is to see the groupings of
# positions, age and caps. Do certain positions lend to younger players 
# while others given to more experienced players?



squads %>% 
  ggplot(aes(age, caps, colour = pos))+
  geom_jitter()


squads %>% 
  ggplot(aes(age, caps, colour = country))+
  geom_point()+
  facet_wrap(~pos)

squads %>%
  filter(
    country == "England" |
      country == "France" |
      country == "Germany" |
      country == "Italy" |
      country == "Netherlands" |
      country == "Norway" |
      country == "Sweden" | country == "United States"
  ) %>%
  ggplot(aes(age, caps, colour = country)) +
  geom_point() +
  facet_wrap(~ pos)

squads %>% 
  filter(grepl(teams, country))

# Y = caps # X = age # colour position



## Changed my mind 

# Library
library(tidyverse)

# Create data
data=data.frame(x=LETTERS[1:26], y=abs(rnorm(26)))

ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


#Isolate 2019 Final

wwc_outcomes2019 <- wwc_outcomes %>% 
  filter(year == 2019 & round == "Final") %>% 
  print()

# Final was United States vs Netherlands lets extract these teams

final_squads <- squads %>%
  filter(country == "US" | country == "Netherlands") %>% 
  group_by(squad_no, country, caps) %>% 
  count(age)

final_squads_US <- final_squads %>% 
  filter(country == "US") %>%
  group_by(squad_no, country) 

final_squads_netherlands <- final_squads %>% 
  filter(country == "Netherlands") %>%
  group_by(squad_no, country) 

final_diff <- final_squads_US

final_diff$age_diff <- final_squads_US$age - final_squads_netherlands$age
final_diff$cap_diff <- final_squads_US$caps - final_squads_netherlands$caps

ggplot(final_diff, aes(x=squad_no, y=age)) +
  geom_segment( aes(x=squad_no , xend=squad_no, y=0, yend=age), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")

