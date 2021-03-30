library(DBI)
library(RSQLite)
library(sqldf)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
#install.packages("colorspace")
library(colorspace)
#install.packages("tidyverse")
library(tidyverse)
library(tweenr)
library(readxl)
library(gganimate)

# Notes
# Run top5_still first to make sure the plot looks ok in "Plots"
# top5_animated will give you a preview of the in "Viewer"
# anim_save() saves the gif ("T5EV Opponent MM/DD/YY.gif") into your current working directory

# Update master, opponent, and date
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

top_ev <- ev[1:5,1:3]
top_ev_copy <- ev[1:5,1:3]

for(i in 1:5){
  top_ev$Batter[i] <- paste(top_ev_copy$Batter[i],i) # this so we can plot the same batter twice
}

# y min and y max
min1 <- floor(min(top_ev$ExitSpeed)/2)*2
max1 <- ceiling(max(top_ev$ExitSpeed))
min2 <- floor(min(top_ev$ExitSpeed)/5)*5 
max2 <- ceiling(max(top_ev$ExitSpeed)/5)*5

top5_still <- ggplot(top_ev,aes(reorder(Batter,-ExitSpeed),ExitSpeed,fill=ExitSpeed))+
  geom_bar(stat="identity",
           color="black")+#,
           #fill=heat.colors(5))+
  scale_fill_gradient(low="yellow",high="red")+
  ggtitle(paste0("Top Exit Velos"))+
  theme_minimal()+
  theme(plot.title = element_text(face="bold",hjust =0.5),
        axis.title =element_text(face="bold",size=14),
        axis.text =element_text(color="black",size="12"))+
  scale_y_continuous(name="Evit Velocity (mph)",breaks = seq(min2,max2,1))+
  coord_cartesian(ylim=c(min1,max1))+
  xlab("Batter")+
  scale_x_discrete(labels=top_ev_copy$Batter)+
  geom_text(aes(label=ExitSpeed),
            position = position_dodge(width = 1),
            vjust=-0.3,
            fontface="bold")+
  labs(caption="UCSB 2019",fill="ExitVelo")+
  labs(caption=paste0("UCSB vs. ", opponent," ",date))

top5_still

top5_animated <- top5_still+
  transition_states(-ExitSpeed)+
  shadow_mark(past=T)+
  enter_drift(y_mod = -3)

top5_animated

anim_save(animation = top5_animated,filename=paste("T5EV ",opponent," ",date,".gif",sep=""))
