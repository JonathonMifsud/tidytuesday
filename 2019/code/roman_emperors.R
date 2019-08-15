# Roman Emperors TidyTuesday
# 13/08/19


library(tidyverse)
library(lubridate)
library(scales)
library(ggraph)
library(igraph)
library(viridis)
library(treemap)
library(data.tree)  

# Data
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

# Cleaning
circle_data <- emperors %>% 
  select(name, cause, reign_start, reign_end, dynasty) %>%
  mutate(span = interval(ymd(reign_start), ymd(reign_end))) %>% 
  mutate(spandays = abs(span %/% days(1))) %>%
  mutate(spanmonths = round(spandays / 30)) %>% # length of reign in days
  mutate(spanyears = round(spanmonths / 12) + 1) %>% # length of reign in months
  select(-reign_start, -reign_end)

# Here I transform the dataframe into a hierarchical structure which is used for circle stacking plots. I was planning to have 3 levels of depth:
# 1. Cause of death 2. Dynasty and 3. Emperor Name and use the length of reign to size the circle but I ran out of time to do so. 

circle_data$pathString <- paste("start", 
                            circle_data$cause, 
                            circle_data$name,
                            sep = "/")
hier_emperors <- as.Node(circle_data, mode = "table")
hier_clone <- Clone(hier_emperors)
hier_network <- ToDataFrameNetwork(hier_clone, "spandays", "spanmonths", "spanyears") #spandays etc is added to the network
mygraph <- graph_from_data_frame(hier_network)

set.seed(123)
gg <- ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth))) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[3], "2" = viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  annotate("text", x = -6,9, y = 5.4, fontface = "bold", size = 5.5, label = "Assassination") + #text annotations
  annotate("text", x = 5.2, y = 6.1, fontface = "bold", size = 5.5, label = "Execution") +
  annotate("text", x = 7.2, y = 1.8, fontface = "bold", size = 5.5, label = "Unknown") +
  annotate("text", x = 5.3, y = -3.2, fontface = "bold", size = 5.5, label = "Captivity") +
  annotate("text", x = 5.4, y = -5.3, fontface = "bold", size = 5.5, label = "Natural Causes") +
  annotate("text", x = -6.4, y = -5.0, fontface = "bold", size = 5.5, label = "Died in Battle") +
  annotate("text", x = 1, y = -0.2, fontface = "bold", size = 5.5, label = "Suicide") +
  theme_void()+
  theme(legend.position="FALSE",
    plot.title = element_text(hjust = 0.5, size = 26),
    plot.subtitle = element_text(hjust = 0.5, size = 22),
    plot.caption = element_text(size = 8,
                                color = "#939184")
  )
 

arrows <- tibble(    
  x2 = c(-4.6,4.3,5.6,4.3,4.3,-5.2),    
  x1 = c(-6,5,6.5,5,5,-6.2),    
  y2 = c(4,4.6,0.4,-2.3,-4.3,-4.3),    
  y1 = c(5,5.7,1.5,-3,-5,-4.8)  
)  

gg1 <-gg +    geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
                       arrow = arrow(length = unit(0.1, "inch")),
                       size = 1, color = "black", curvature = 0.15)  

ggfull <- gg1 + labs(title = "Roman Emperors:", subtitle = "How they meet their end", caption = "Author: @jonathon_mifsud, Source: Wikipedia / Zonination") 
ggsave(
  "emperors.png",
  plot = ggfull,
  width = 40,
  height = 30,
  units = "cm"
)



















## Drafts ##
data <- emperors %>% 
  select(name, cause, reign_start, reign_end, dynasty) %>%
  mutate(span = interval(ymd(reign_start), ymd(reign_end))) %>% 
  mutate(spandays = abs(span %/% days(1))) %>% 
  mutate(spanmonths = round(spandays / 30)) %>% 
  mutate(spanyears = round(spanmonths / 12) + 1)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$cause), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$cause <- rep(levels(data$cause), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(cause)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(cause) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=spanyears, fill=cause)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=spanyears, fill=cause), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 33, xend = start, yend = 33), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 23, xend = start, yend = 23), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 13, xend = start, yend = 13), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 3, xend = start, yend = 3), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(3, 13, 23, 33), label = c("100", "200", "300", "400") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=spanyears, fill=cause), stat="identity", alpha=0.5) +
  ylim(-100,40) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=spanyears+10, label=name), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+
  
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=cause), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p

hjust <- c(0,0,1,1)



