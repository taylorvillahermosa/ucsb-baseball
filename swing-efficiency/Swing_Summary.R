library(dplyr)
source("/Users/taylorvillahermosa/Desktop/ Baseball/Swing Efficiency/Plate Discipline.R")
# utilizes add.StrikeZone() and percent.format() from "Plate Discipline.R"

ultimate <- read.csv("/Users/taylorvillahermosa/Desktop/ Baseball/Ultimate/Ultimate_Excel.csv")

SwingSummary <- add.StrikeZone(ultimate) %>%
  mutate(Date = as.Date(Date,"%m/%d/%y")) %>%
  filter(BatterTeam %in% c("UCSB_GAU", "SAN_GAU"), 
         Date > "2018-09-19",
         !TaggedPitchType %in% c("Changeup", "Fastball"),
         Batter %in% c("Castanon, Marcos", "Johnson, Kyle", "Kirtley, Christian", 
                       "Mueller, Cole", "O'Connor, McClain", "Willow, Jason")) %>%
  mutate(BatterTeam = "UCSB_GAU",
         Outside = ifelse(StrikeZone == F, 1, 0),
         Inside = ifelse(StrikeZone == T, 1, 0),
         Swing = ifelse(PitchCall %in% c("StrikeCalled", "BallCalled"), 0, 1),
         Swing.Outside = ifelse(StrikeZone == F & Swing == 1, 1, 0),
         Swing.Inside = ifelse(StrikeZone == T & Swing == 1, 1, 0),
         Whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0),
         Contact = ifelse(PitchCall %in% c("InPlay","FoulBall"), 1, 0),
         Contact.Inside = ifelse(Contact == T & StrikeZone == T, 1, 0),
         Contact.Outside = ifelse(Contact == T & StrikeZone == F, 1, 0)) %>%
  group_by(Batter) %>%
  summarise(`O Swing` = sum(Swing.Outside, na.rm = T)/sum(Outside, na.rm = T),
            `Z Swing` = sum(Swing.Inside, na.rm = T)/sum(Inside, na.rm = T) ,
            `Swing` = sum(Swing, na.rm = T)/n(),
            `O Contact` = sum(Contact.Outside, na.rm = T)/sum(Outside, na.rm = T),
            `Z Contact` = sum(Contact.Inside, na.rm = T)/sum(Inside, na.rm = T),
            `Contact` = sum(Contact, na.rm = T)/n(),
            `Zone` = sum(Inside, na.rm = T)/n(),
            `Whiff` = sum(Whiff, na.rm = T)/n()) %>%
  mutate_if(is.numeric, percent.format)

# SwingSummary is in this format:
# Batter      O Swing   Z Swing   Swing   O Contact   Z Contact   Contact   Zone    Whiff
# Batter1     30%       68%       48%     21%         60%         82%       48%     9%
# Batter2     29%       59%       43%     23%         46%         78%       48%     9%
# Batter3     21%       68%       43%     11%         54%         74%       48%     11%


# SwingSummaryTransposed is in this format:

#               Batter1   Batter2   Batter3
# O Swing       30%       29%       21%
# Z Swing       68%       59%       68%
# Swing         48%       43%       43%
# O Contact     21%       23%       11%    
# Z Contact     21%       23%       11%
# Contact       82%       78%       74%
# Zone          48%       48%       48%
# Whiff         9%        9%        11%

swing.data.transposed <- data.frame(t(SwingSummary[,-1]))  # SwingSummary values transposed
names <- as.character(unlist(SwingSummary[1]))  # takes names of batters as a vector of characters
SwingSummaryTransposed <- setNames(swing.data.transposed, names)  #  makes names of batters as column headings
 write.csv(SwingSummaryTransposed, "Swing Summary Team.csv")
 