# Tidytuesday 03/09/2019
# Moore's Law
# Jonathon Mifsud

library(tidyverse)
library(showtext)
library(cowplot)
library(ggthemes)
library(showtext)


#data
cpu <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv"
  )
gpu <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv"
  )
ram <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv"
  )


#loading in Oswald font
font_add_google(name = "Oswald", family = "Oswald")
showtext_auto()

#assigning plot colors
gpu_col <- "#003f5c"
cpu_col <- "#bc5090"
ram_col <- "#ffa600"

#theme used for the three plots
mytheme <- theme(
  panel.spacing.x =  unit(12, "pt"),
  panel.grid = element_blank(),
  plot.title = element_text(size = 68, hjust = 0.5),
  axis.text.x = element_text(face = "bold", size = 42, color = "grey40"),
  text = element_text(family = "Oswald")
)


#Plot1
pcpu <- cpu %>%
  ggplot(aes(y = transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21,
             alpha = 0.6,
             fill = cpu_col) +
  geom_smooth(color = "white",
              alpha = 0.5,
              fill = cpu_col)+

  scale_x_continuous(breaks = c(1971, 1987, 2003, 2019),
                     limits = c(1963, 2025)) +
  scale_y_log10(breaks = c(1, 10 ^ 3, 10 ^ 6, 10 ^ 9, 10 ^ 12),
                labels = scales::comma) +
  coord_cartesian(ylim = c(1, 2 * 10 ^ 12)) +
  theme_fivethirtyeight() +
  mytheme +
  theme(
    axis.text.y = element_text(face = "bold", size = 42, color = "grey40"),
    axis.title = element_text(),
    axis.title.y = element_text(size = 68, face = "bold")
  ) +
  panel_border(
    colour = "black",
    size = 0.5,
    linetype = 1,
    remove = FALSE
  )

#Plot1 Labels
pcpu <-
  pcpu + labs(x = NULL, y = 'Number of Transistors', title = "CPU")


#Plot2
pgpu <- gpu %>%
  ggplot(aes(y = transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21,
             alpha = 0.6,
             fill = gpu_col) +
  geom_smooth(color = "white",
              alpha = 0.5,
              fill = gpu_col, method = "lm") +
  scale_x_continuous(breaks = c(1971, 1987, 2003, 2019),
                     limits = c(1963, 2025)) +
  scale_y_log10(breaks = c(1, 10 ^ 3, 10 ^ 6, 10 ^ 9, 10 ^ 12),
                labels = scales::comma) +
  coord_cartesian(ylim = c(1, 2 * 10 ^ 12)) +
  theme_fivethirtyeight() +
  mytheme +
  theme(axis.text.y = element_blank()) + #removing the y axis text for this plot
  panel_border(
    colour = "black",
    size = 0.5,
    linetype = 1,
    remove = FALSE
  )

#Plot2 Labels
pgpu <- pgpu + labs(x = NULL, y = NULL, title = "GPU")


pram <- ram %>%
  ggplot(aes(y = transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21,
             alpha = 0.6,
             fill = ram_col) +
  geom_smooth(color = "white",
              alpha = 0.5,
              fill = ram_col) +
  scale_x_continuous(breaks = c(1971, 1987, 2003, 2019),
                     limits = c(1963, 2025)) +
  scale_y_log10(breaks = c(1, 10 ^ 3, 10 ^ 6, 10 ^ 9, 10 ^ 12),
                labels = scales::comma) +
  coord_cartesian(ylim = c(1, 2 * 10 ^ 12)) +
  theme_fivethirtyeight() +
  mytheme +
  theme(axis.text.y = element_blank()) + #removing the y axis text for this plot
  panel_border(
    colour = "black",
    size = 0.5,
    linetype = 1,
    remove = FALSE
  )

pram <- pram + labs(x = NULL, y = NULL, title = "RAM")

plot_row <-
  plot_grid(
    pcpu,
    pgpu,
    pram,
    ncol = 3,
    nrow = 1,
    rel_widths = c(1.4, 1, 1)
  )


#Each of the three plots have there own titles e.g. "Ram", "CPU" but we need an overall plot title for when these plots are joined together using cowplot
title <- ggdraw() +
  draw_label("Moore's Law", #main title
             fontface = 'bold',
             x = 0,
             hjust = 0, vjust= -0.01,size = 120) +
  draw_label("Does the number of transistors double each year?", #subtitle
             fontface = 'bold',
             x = 0,
             hjust = 0, vjust = 2.3, size = 72) +
  theme_void()+ 
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 130),
    text = element_text(family = "Oswald"),
    plot.background = element_rect(fill = "#F0F0F0", color = NA)
  )

finalplot <- plot_grid(title,
                       plot_row,
                       ncol = 1,
                       # rel_heights values control vertical title margins
                       rel_heights = c(0.1, 1))


ggsave(
  "mooreslaw.png",
  plot = finalplot,
  width = 40,
  height = 30,
  units = "cm",
  dpi = "retina"
)

