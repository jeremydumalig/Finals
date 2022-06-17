library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
rm(list=ls())

title <- "NBA Finals Game 6: Warriors at Celtics"
c_away <- "#1D428A"
c_home <- "#007A33"

# 
get_max <- function(col){
  col_max <-  max(col)
  col_min <- min(col)
  
  max(col_max, abs(col_min))
}

# 
lead <- read_csv(file="lead.csv")
lead <- rbind(lead,
              mutate(slice(lead, rep(n():n(), each=9)), 
                     Lead=0))

# 
end_quarters <- lead %>%
  group_by(Quarter) %>%
  summarise(end = max(`...1`)) %>%
  ungroup()

# 
lead_tracker <- 
  ggplot(lead, aes(`...1`, Lead, fill=Team, group=`...1`)) +
  geom_bar(stat="identity", 
           width=2,
           show.legend=FALSE) +
  scale_fill_manual(values=c(c_home, c_away)) +
  geom_vline(xintercept = end_quarters$end,
             linetype="dashed") +
  labs(title=title,
       x="Quarter") +
  ylim(-get_max(lead$Lead), get_max(lead$Lead)) +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.background = element_rect(color="black"),
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=15),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

#
lead_tracker

#
animate(lead_tracker + 
          transition_reveal(`...1`) + 
          ease_aes('sine-in-out'), 
        nframes=nrow(lead),
        duration=20)

# 
anim_save("lead.gif")