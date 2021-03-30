library(DBI)
library(RSQLite)
library(sqldf)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(colorspace)
library(tidyverse)
library(tweenr)
library(readxl)
library(gganimate)
setwd("/Users/taylorvillahermosa/Desktop")

# Update master, opponent, date
master <- read_excel("/Users/taylorvillahermosa/Desktop/Ultimate_Excel.xlsx")
master$Date <- as.Date(master$Date, format="%Y-%m-%d")
master <- subset(master, Date >= "2019-02-16")
#opponent <- "CBU"
date <- "2019"

master_db <- dbConnect(RSQLite::SQLite(), "master_db.sqlite")

dbWriteTable(master_db,"MASTER",master,overwrite=TRUE)

hits <- as.data.frame(dbGetQuery(master_db,'SELECT ExitSpeed, Distance, PlayResult, Batter
                                    FROM "MASTER" 
                                    WHERE "BatterTeam" IN ("SAN_GAU","UCSB_GAU") AND 
                                          "ExitSpeed" != "NA" AND
                                          "PlayResult" NOT IN ("Out","Undefined","Error")'))

hits$ExitSpeed <- round(hits$ExitSpeed, digits = 1)
hits$Distance <- round(hits$Distance, digits = 1)

first_name <- sub("(\\w+),\\s(\\w+)","\\2", hits$Batter)  
last_name <- sub("(\\w+),\\s(\\w+)","\\1", hits$Batter)  
hits$Batter <- paste(substr(first_name,1,1),". ",last_name,sep="")  # first initial last name
hits$PlayResult <- factor(hits$PlayResult, levels=c("Single","Double","Triple","HomeRun","Out"))
hits <- na.omit(hits)

x<-ggplot(hits,aes(Distance, ExitSpeed,shape=PlayResult,color=PlayResult,label=Batter, group=Distance))+
  geom_point(size=3)+
  scale_color_manual(values=c("blue","yellow","green","red"))+
  scale_shape_manual(values=c(20,15,17,18))+
  #geom_text_repel(fontface="bold",point.padding=.1)+
  #theme_bw()+
  ggtitle("Exit Velos vs. Distance")+
  theme(plot.title = element_text(face="bold",hjust =0.5,size=14),
        axis.title =element_text(face="bold",size=12))+
  xlab("Distance (ft)")+
  ylab("Exit Velocity (mph)")+
  scale_fill_discrete(name = "Result")+
  labs(caption=paste0("UCSB ",date))

x_animated <- x +
  transition_reveal(Distance)+
  enter_fade()+
  exit_fade()
x_animated

anim_save(animation = x_animated,filename=paste("EV vs Dist 2019.gif",sep=""))
