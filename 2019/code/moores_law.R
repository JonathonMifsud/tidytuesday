# Tidytuesday 03/09/2019
# Moore's Law
# Jonathon Mifsud

cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")

library(tidyverse)
library(ggrepel)
library(showtext)
library(ggpomological)
library(cowplot)
font_add_google(name = "Caveat", family = "Caveat")

mytheme <- theme(
  text = element_text(family = "Caveat"), # handwritten font
  plot.title = element_text(size = 32),
  plot.subtitle = element_text(size = 24),
  axis.title.x = element_text(size = 32, hjust = 0.5),
  axis.title.y = element_text(size = 32, hjust = 0.5),
  axis.text = element_text(size = 24)
)

gpu_col <- "#003f5c"
cpu_col <- "#bc5090"
ram_col <- "#ffa600"


pgpu <- gpu %>% 
  mutate(transistor_count = log10(transistor_count)) %>% 
  ggplot(aes(y= transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21, alpha = 0.6, fill = gpu_col)+
  geom_smooth(color = "white", alpha = 0.5, fill = gpu_col)
  #geom_text_repel()

pcpu <- cpu %>% 
  mutate(transistor_count = log10(transistor_count)) %>% 
  ggplot(aes(y = transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21, alpha = 0.6, fill = cpu_col)+
  geom_smooth(color = "white", alpha = 0.5, fill = cpu_col)

pram <- ram %>% 
  mutate(transistor_count = log10(transistor_count)) %>% 
  ggplot(aes(y = transistor_count, x = date_of_introduction)) +
  geom_point(shape = 21, alpha = 0.6, fill = ram_col)+
  geom_smooth(color = "white", alpha = 0.5, fill = ram_col)

plot_grid(pgpu, pcpu, pram, ncol = 3, nrow = 1)


