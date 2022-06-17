library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
rm(list=ls())

title <- "NBA Finals Game 6: Warriors at Celtics"
filename <- "finals6"
away <- "GSW"
home <- "BOS"
c_away <- "#1D428A"
c_home <- "#007A33"

# 
get_caption <- function(frame){
  paste( "Quarter: ", game[frame,]$Quarter, 
         ", Time: ", game[frame,]$Clock, 
         sep="")
}

# 
game <- read_csv(file=paste(filename, ".csv", sep=""))

# 
gg_game <- ggplot(game, aes(x=Team, y=Score, fill=Team)) +
  geom_bar(stat='identity',
           show.legend=FALSE) + 
  scale_fill_manual(values=c(c_away, c_home)) +
  scale_x_discrete(labels = c(away, home)) +
  labs(title=title,
       x="{game[frame,]$SCORE}",
       caption="{get_caption(frame)}") +
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
    axis.title.y = element_text(size=15)
  )

# 
animate(gg_game + 
          transition_states(`...1`, 
                            transition_length=3, 
                            state_length=1) + 
          ease_aes('sine-in-out'), 
        nframes=nrow(game),
        duration=30)

# 
anim_save(paste(filename, ".gif", sep=""))
