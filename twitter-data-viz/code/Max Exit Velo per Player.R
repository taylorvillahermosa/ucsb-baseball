library(DBI)
library(RSQLite)
library(sqldf)
library(readxl)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(tweenr)
library(gganimate)

# Notes
# Run max_ev_still first to make sure the plot looks ok in "Plots"
# max_ev_animated will give you a preview of gif the in "Viewer"
# anim_save() saves the gif ("MaxPlayerEV Opponent MM/DD/YY.gif") into your current working directory
# if the names in the gif are too squished increase the width/height in anim_save() to 600

# Change master, opponent, and date
#master <- read_excel("/Users/taylorvillahermosa/Desktop/2019 Season Excel.xlsx")
master <- read.csv("/Users/taylorvillahermosa/Desktop/Hartford1Excel.csv")
opponent <- "Hartford"
date <- "2-22-19"  # use - (DO NOT USE / to separate date)

exit_velo_db <- dbConnect(RSQLite::SQLite(), "exit_velo_db.sqlite")
dbWriteTable(exit_velo_db,"Exit Velo",master,overwrite=TRUE)

ev <- as.data.frame(dbGetQuery(exit_velo_db,'SELECT Batter, ExitSpeed, PlayResult
                               FROM "Exit Velo" 
                               WHERE "PitchCall" = "InPlay" AND "BatterTeam" IN ("SAN_GAU","UCSB_GAU")
                               ORDER BY ExitSpeed DESC'))
ev<- na.omit(ev)
ev$ExitSpeed <- round(ev$ExitSpeed,digits = 1)
first_name <- sub("(\\w+),\\s(\\w+)","\\2", ev$Batter)  
last_name <- sub("(\\w+),\\s(\\w+)","\\1", ev$Batter)
ev$Batter <- paste(substr(first_name,1,1),". ",last_name,sep="")  # first initial last name

max_ev <- subset(ev, !duplicated(ev$Batter))  # each batter's maximum exit velo

max <- ceiling(max(max_ev$ExitSpeed)/5)*5
min <- floor(min(max_ev$ExitSpeed)/5)*5 

max_ev_still <- ggplot(max_ev,aes(reorder(Batter,-ExitSpeed),ExitSpeed,fill=ExitSpeed))+
  geom_bar(stat="identity",color="black")+
  scale_fill_gradient(low="yellow",high="red")+
  ggtitle("Max Exit Velos")+
  theme_minimal()+
  theme(plot.title = element_text(face="bold",hjust =0.5),
        axis.title =element_text(face="bold",size=14),
        axis.text =element_text(color="black",size=10))+
  scale_y_continuous(name="Evit Velocity (mph)",breaks = seq(min,max,5))+
  coord_cartesian(ylim=c(min,max))+
  xlab("Batter")+
  geom_text(aes(label=ExitSpeed),
            position = position_dodge(width = 1),
            vjust=-0.3,
            fontface="bold")+
  labs(caption=paste0("UCSB vs. ", opponent," ",date),fill="ExitVelo")

max_ev_still

max_ev_animated <- max_ev_still+
  transition_states(-ExitSpeed)+
  shadow_mark(past=T)+
  enter_drift(y_mod = -3)

max_ev_animated

anim_save(animation = max_ev_animated,filename = paste("MaxPlayerEV ",opponent," ",date,".gif",sep=""),width=560,height=560)

