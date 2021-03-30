library(dplyr)
library(ggplot2)
source("/Users/taylorvillahermosa/Desktop/ Baseball/Swing Efficiency/Plate Discipline.R")
master <- read.csv("/Users/taylorvillahermosa/Desktop/Ultimate_Excel_Current (11_25_19).csv")

# filtering to only called pitches
ultimate <- master %>% 
  mutate(Date = as.Date(Date,"%m/%d/%y")) %>%
  filter(Date > "2019-10-01",
         PitchCall %in% c("StrikeCalled", "BallCalled"))

# adding TP/FP/TN/FN
ultimate <- add.StrikeZone(ultimate) %>%
  mutate(Framing = ifelse(PitchCall == "BallCalled" & StrikeZone == TRUE, "Ball (Zone)", 
                          ifelse(PitchCall == "StrikeCalled" & StrikeZone == FALSE, "Strike (O.Zone)", 
                                 ifelse(PitchCall == "BallCalled" & StrikeZone == FALSE, "Ball (O.Zone)", "Strike (Zone)"))))

# confusion matrix for 1 player
Martz <- ultimate %>% filter(Catcher == "Martz")
Martz.confusion.matrix <- table(droplevels(Martz)$PitchCall, Martz$StrikeZone)
Martz.confusion.matrix

# data for each catcher
catchers <- ultimate %>% 
  group_by(Catcher) %>% 
  summarise(`50% K, StrikeCalled` = sum(StrikeZone == TRUE & PitchCall == "StrikeCalled"),
            `50% K, BallCalled` = sum(StrikeZone == TRUE & PitchCall == "BallCalled"),
            `50% B, StrikeCalled` = sum(StrikeZone == FALSE & PitchCall == "StrikeCalled"),
            `50% B, BallCalled` = sum(StrikeZone == FALSE & PitchCall == "BallCalled"),
            ZoneKRate = `50% K, StrikeCalled`/(`50% K, StrikeCalled` + `50% K, BallCalled`),
            O.ZoneKRate = `50% B, StrikeCalled`/(`50% B, StrikeCalled` + `50% B, BallCalled`))
catchers

# plot formatting
theme.formatting <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(colour="lightblue", size=0.3, linetype = "dashed"),
        panel.grid.minor = element_line(colour="lightblue", size=0.3, linetype = "dashed"))

scales <- list(scale_x_continuous(breaks = seq(-20,20,10), minor_breaks = seq(-20 , 20, 2)),
               scale_y_continuous(breaks = seq(10,60,10), minor_breaks = seq(0, 60, 2)),
               coord_fixed(xlim=c(-20,20),ylim=c(5,60)))

labels <- labs(x = "Horizontal Location (in)", y = "Vertical Location (in)")
K.rectangle <- geom_rect(mapping = aes(xmin = -8.5, xmax = 8.5, ymin = 20, ymax = 42), 
                         fill = NA, color = "black", alpha = 0.5)


# vs RHH plot
ggplot(RHH.strike.predict.data %>% filter(strike.prob >= .5))+
  stat_ellipse(aes(PLS,PLH), level=.89, color = "red", size = 1) + 
  geom_point(data = ultimate %>% filter(Catcher == "", BatterSide == "Right"), 
             aes(PLS, PLH, color = Framing),
             size = 1, stroke = 1) + 
  scale_shape_manual(values = c(1, 4)) +
  scale_color_manual(values = c("black", "red", "blue", "black")) +
  theme.formatting + labels + scales + K.rectangle + 
  ggtitle(" vs. RHH") 

# vs LHH plot
ggplot(LHH.strike.predict.data %>% filter(strike.prob >= .5))+
  stat_ellipse(aes(PLS, PLH), level=.89, color = "red", size = 1) + 
  geom_point(data = ultimate %>% filter(Catcher == "", BatterSide == "Left"), 
             aes(PLS, PLH, color = Framing),
             size = 1, stroke = 1) + 
  scale_shape_manual(values = c(1, 4)) +
  scale_color_manual(values = c("black", "red", "blue", "black")) +
  theme.formatting + labels + scales + K.rectangle + 
  ggtitle(" vs. LHH") 
