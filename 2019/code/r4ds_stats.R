#############################################################
##    R for Data Science Online Learning Community Stats   ##
##                       Jonathon Mifsud                   ##
#############################################################

library(tidyverse)
library(reshape2)
library(emojifont)
library(ggthemes)


r4ds_members <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv"
  )

theme <- theme_minimal()

## After looking at the data I am planning to examine which day slack is the busiest

r4ds_members <-  r4ds_members %>%
  filter(total_membership >= 428) %>% #removing slack early membership
  mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>%
  mutate(daily_quiet_members = daily_active_members - daily_members_posting_messages)

r4ds_day <- r4ds_members %>%
  select(day,
         daily_members_posting_messages,
         daily_quiet_members) %>%
  group_by(day) %>%
  summarise(
    posting_members = round(sum(daily_members_posting_messages)),
    shy_members = round(sum(daily_quiet_members)),
    day_count = n()
  )

r4ds_melt <- r4ds_day %>%
  select(-day_count) %>%
  melt()

p <- r4ds_melt %>%
  ggplot(aes(day, value, fill = variable)) +
  geom_chicklet(width = .6, radius = grid::unit(5, "pt")) +
  theme_minimal() +
  ggthemes::scale_fill_tableau('Color Blind',
                               name = "Type of R4DS Member:",
                               labels = c("Poster", "Shy")) +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = "bottom",
    legend.title = element_text(color = "grey30", size = 14, face = "bold"),
    legend.text = element_text(
      color = "grey30",
      size = 12,
      face = "bold",
      margin = margin(t = 10)
    ),
    legend.spacing.x = unit(0.8, "cm"),
    legend.spacing.y = NULL,
    axis.text.y = element_text(color = "grey30", size = 12),
    axis.text.x = element_text(color = "grey30", size = 12),
    axis.title.y = element_text(
      color = "grey30",
      size = 16,
      face = "bold",
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(color = "grey30", size = 12, face = "bold"),
    plot.title = element_text(color = "grey30", size = 22, face = "bold"),
    plot.subtitle = element_text(size = 14)
  ) +
  labs(
    x = NULL ,
    y = "Total Number of Active Members Overtime",
    title = "User activity on R4DS Slack",
    subtitle = "By day of the week, segmented by whether the user contributes a message/post or just reads (shy)",
    caption = "Source R4DS Slack, Plot by @jonathon_mifsud"
  ) +
  guides(
    fill = guide_legend(
      title = "Type of R4DS Member:",
      label.position = "right",
      label.hjust = 0.5,
      title.position = "top",
      title.vjust = 0.3
    )
  )
p




## For exporting (problems with font size)
p2 <- r4ds_melt %>%
  ggplot(aes(day, value, fill = variable)) +
  geom_chicklet(width = .6, radius = grid::unit(5, "pt")) +
  theme_minimal() +
  ggthemes::scale_fill_tableau('Color Blind',
                               name = "Type of R4DS Member:",
                               labels = c("Poster", "Shy")) +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = "bottom",
    legend.title = element_text(color = "grey30", size = 32, face = "bold"),
    legend.text = element_text(
      color = "grey30",
      size = 32,
      face = "bold",
      margin = margin(t = 10)
    ),
    legend.spacing.x = unit(0.8, "cm"),
    legend.spacing.y = NULL,
    axis.text.y = element_text(color = "grey30", size = 30),
    axis.text.x = element_text(color = "grey30", size = 40),
    axis.title.y = element_text(
      color = "grey30",
      size = 40,
      face = "bold",
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(color = "grey30", size = 40, face = "bold"),
    plot.title = element_text(color = "grey30", size = 60, face = "bold"),
    plot.subtitle = element_text(size = 40),
    plot.caption = element_text(size = 18)
  ) +
  labs(
    x = NULL ,
    y = "Total Number of Active Members Overtime",
    title = "User activity on R4DS Slack",
    subtitle = "By day of the week, segmented by whether the user contributes a message/post or just reads (shy)",
    caption = "Source R4DS Slack, Plot by @jonathon_mifsud"
  ) +
  guides(
    fill = guide_legend(
      title = "Type of R4DS Member:",
      label.position = "right",
      label.hjust = 0.5,
      title.position = "top",
      title.vjust = 0.3
    )
  )
p2



ggsave("2019/plots/plot_2019-07-16.png", width = 29, height = 21, units = "cm", dpi = "retina")
