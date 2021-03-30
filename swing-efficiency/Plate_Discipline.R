library(dplyr)
library(mgcv)
library(SIBER)

###################################################  Plate Discipline Statistics  ###################################################

# This code contains the following functions:
# -----------------------------------------------------------------------------------------------------------------------------------
#                       |                       |  returns  |                                                                       |
#       function        |      parameters       |   type    |                              result                                   |
# ----------------------|-----------------------|-----------|------------------------------------------------------------------------
#     StrikeZone()      |  PLS, PLH, BatterSide |  bool     |   TRUE/FALSE for inside/outside strike zone                           |
#     add.StrikeZone()  |  TrackMan df          |  df       |   adds StrikeZone (T = in, F = out) and Swing column (Swing/Take)     |
#     o.swing()         |  TrackMan df          |  numeric  |   swing % pitches outside of the zone (chase rate)                    |
#     z.swing()         |  TrackMan df          |  numeric  |   swing % pitches inside the zone                                     |
#     swing()           |  TrackMan df          |  numeric  |   swings % all pitches                                                |
#     o.contact()       |  TrackMan df          |  numeric  |   contact % pitches outside of the zone                               |
#     z.contact()       |  TrackMan df          |  numeric  |   contact % pitches inside of the zone                                |
#     contact()         |  TrackMan df          |  numeric  |   contact % all pitches                                               |
#     zone()            |  TrackMan df          |  numeric  |   % pitches inside the zone                                           |
#     whiff()           |  TrackMan df          |  numeric  |   whiff %                                                             |
#     percent.format()  |  decimal              |  char     |   converts 0.XX to XX% (rounded to nearest integer)                   |
#     swing.summary()   |  TrackMan df          |  vector   |   vector of all plate discipline statistics in percent format         |
# ----------------------------------------------------------------------------------------------------------------------------------

# These metrics are based on a strike zone ellipse where pitches have ≥ 50% probability of being called strikes.
# The 50% probability strike zones are created using Ultimate_Excel 6/17/19. In order to make a strike zone 
# using other TrackMan data/strike probabilities and for an explanation of how the strike zones are constructed, 
# consult "Strike Zones.rmd".

####################################################  Strike Zone Calculations  #####################################################

# subsetting ultimate to only called strikes and balls:
called.pitches <- read.csv("/Users/taylorvillahermosa/Desktop/ Baseball/Ultimate/Ultimate_Excel.csv") %>%
  mutate(PLS = 12*PlateLocSide, PLH = 12*PlateLocHeight,
         strike.called = ifelse(PitchCall == "StrikeCalled",1,0),
         swing = ifelse(PitchCall %in% c("FoulBall", "InPlay", "StrikeSwinging"),"Swing","Take")) %>% 
  filter(PLH > 0, PLS < 35, PLS > -35, !is.na(PLS),
         PitchCall %in% c("StrikeCalled", "BallCalled"))

#  50% strike probability
k.prob <- .5

# percentile of the ellipse to fit around the strike zone
p <- .87

#  (horz, vert) location coordinates for every square inch
x <- seq(-18, 18, length.out=37)
z <- seq(6, 60, length.out=55)
location.coordinates <- data.frame(PLS = c(outer(x, z * 0 + 1)),
                                   PLH = c(outer(x * 0 + 1, z)))

# RHH 50% Called Strike Probability Strike Zone
# predicting strike probability for each location
RHH.strike.zone.data <- called.pitches %>% filter(BatterSide == "Right")
RHH.strike.model.fit <- gam(strike.called ~ s(PLS, PLH,k=10), 
                            family = binomial, 
                            data = RHH.strike.zone.data)
RHH.strike.predict.data <- location.coordinates
RHH.strike.model.predict <- predict(RHH.strike.model.fit, RHH.strike.predict.data)
RHH.strike.predict.data <- RHH.strike.predict.data %>%
  mutate(strike.prob = exp(RHH.strike.model.predict) / (1 + exp(RHH.strike.model.predict)))

#  data frame of RHH 50% strike probability locations
RHH.X <- RHH.strike.predict.data %>% filter(strike.prob >= k.prob) %>% select(PLS)
RHH.Y <- RHH.strike.predict.data %>% filter(strike.prob >= k.prob) %>% select(PLH)
RHH.XY <- na.omit(data.frame(RHH.X,RHH.Y))

#  ellipse calculations
mu.RHH <- colMeans(RHH.XY) # center of the ellipse
Sigma.RHH <- cov(RHH.XY) # covariance matrix of the ellipse


# LHH 50% Called Strike Probability Strike Zone
# predicting strike probability for each location
LHH.strike.zone.data <- called.pitches %>% filter(BatterSide == "Left")
LHH.strike.model.fit <- gam(strike.called ~ s(PLS, PLH,k=10), 
                            family = binomial, 
                            data = LHH.strike.zone.data)
LHH.strike.predict.data <- location.coordinates
LHH.strike.model.predict <- predict(LHH.strike.model.fit, LHH.strike.predict.data)
LHH.strike.predict.data <- LHH.strike.predict.data %>%
  mutate(strike.prob = exp(LHH.strike.model.predict) / (1 + exp(LHH.strike.model.predict)))

#  data frame of LHH 50% strike probability locations
LHH.X <- LHH.strike.predict.data %>% filter(strike.prob >= k.prob) %>% select(PLS)
LHH.Y <- LHH.strike.predict.data %>% filter(strike.prob >= k.prob) %>% select(PLH)
LHH.XY <- na.omit(data.frame(LHH.X,LHH.Y))

#  ellipse calculations
mu.LHH <- colMeans(LHH.XY) # center of the ellipse
Sigma.LHH <- cov(LHH.XY) # covariance matrix of the ellipse


############################################################  Functions  ############################################################

# StrikeZone(PLS, PLH, BatterSide) 
# returns:
# TRUE = pitch is inside 50% probability strike zone
# False = pitch is outside 50% probability strike zone
StrikeZone <- function(PLS, PLH, BatterSide){
  pitch.location <- data.frame(PLS, PLH)
  
  if(BatterSide == "Right"){
    StrikeZone = ellipseInOut(pointsToEllipsoid(pitch.location, Sigma.RHH, mu.RHH), p = p)
  } else{
    StrikeZone = ellipseInOut(pointsToEllipsoid(pitch.location, Sigma.LHH, mu.LHH), p = p)
  }
  
  return(StrikeZone)
}

# add.StrikeZone(TrackMan.data) 
# returns TrackMan data frame with added column "StrikeZone"
# TRUE = pitch is inside 50% probability strike zone
# False = pitch is outside 50% probability strike zone

add.StrikeZone <- function(TrackMan.data){
  newTrackMan.data <- TrackMan.data %>% 
    mutate(PLS = 12*PlateLocSide, PLH = 12*PlateLocHeight,
           Swing = ifelse(PitchCall %in% c("FoulBall", "InPlay", "StrikeSwinging"),"Swing","Take")) %>%
    filter(!is.na(PLS)) %>%
    rowwise() %>% 
    mutate(StrikeZone = ifelse(BatterSide == "Right",
                               ellipseInOut(pointsToEllipsoid(data.frame(PLS,PLH), Sigma.RHH, mu.RHH), p = p),
                               ellipseInOut(pointsToEllipsoid(data.frame(PLS,PLH), Sigma.LHH, mu.LHH), p = p)))
    return(newTrackMan.data)
}

# Plate Discipline functions
# each takes in a TrackMan data frame
# each returns percentage as a decimal

o.swing <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.chase <- nrow(TrackMan.data %>% filter(Swing == "Swing",StrikeZone==F))
  n.outside <- nrow(TrackMan.data %>% filter(StrikeZone == F))
  return(n.chase/n.outside)
}

z.swing <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.zswing <- nrow(TrackMan.data %>% filter(Swing == "Swing",StrikeZone==T)) 
  n.inside <- nrow(TrackMan.data %>% filter(StrikeZone == T))
  return(n.zswing/n.inside)
}

swing <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.swing <- nrow(TrackMan.data %>% filter(Swing == "Swing")) 
  n.pitch <- nrow(TrackMan.data)
  return(n.swing/n.pitch)
}

o.contact <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.contact.outside <- nrow(TrackMan.data %>% filter(PitchCall %in% c("InPlay","FoulBall"), StrikeZone == F))
  n.outside <- nrow(TrackMan.data %>% filter(StrikeZone == F))
  return(n.contact.outside/n.outside)
}

z.contact <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.inside <- nrow(TrackMan.data %>% filter(StrikeZone == T))
  n.contact.inside <- nrow(TrackMan.data %>% filter(PitchCall %in% c("InPlay","FoulBall"), StrikeZone == T))
  return(n.contact.inside/n.inside)
}

contact <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.contact <- nrow(TrackMan.data %>% filter(PitchCall %in% c("InPlay","FoulBall")))
  n.swing <- nrow(TrackMan.data %>% filter(Swing == "Swing")) 
  return(n.contact/n.swing)
}

zone <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.inside <- nrow(TrackMan.data %>% filter(StrikeZone == T))
  n.pitch <- nrow(TrackMan.data)
  return(n.inside/n.pitch)
}

whiff <- function(TrackMan.data){
  TrackMan.data = add.StrikeZone(TrackMan.data)
  n.whiff <- nrow(TrackMan.data %>% filter(PitchCall == "StrikeSwinging"))
  n.pitch <- nrow(TrackMan.data)
  return(n.whiff/n.pitch)
}

# percent.format(x) reformats decimal as a character "XX%"
# 0.23456 --> 23%
percent.format <- function(x){
  return(paste0(round(100*x),"%"))
}

# swing.summary() takes in TrackMan.data and returns a vector of plate discipline stats in % format
# filter TrackMan.data by an individual player before calling function
swing.summary <- function(TrackMan.data){
  return(percent.format(c(o.swing(TrackMan.data), z.swing(TrackMan.data), 
                          swing(TrackMan.data), o.contact(TrackMan.data),
                          z.contact(TrackMan.data), contact(TrackMan.data),
                          zone(TrackMan.data),whiff(TrackMan.data))))
}
