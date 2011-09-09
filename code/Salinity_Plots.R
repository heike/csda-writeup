
#SALINITY PLOTS
# Lendie's path
setwd("C:\\Users\\Owner\\Documents\\csda-writeup")

# libraries
library(ggplot2)
library(gridExtra)


# load data
salinity <- read.csv("data/Salinity.csv")
salinity$Date_Time <- as.Date(as.character(salinity$Date_Time), "%Y-%m-%d")
#map
library(maps)
states <- map_data("state")
states.animal <- getbox(states, xlim=c(-97, max(animal$Longitude+2)), ylim=range(animal$Latitude))
# state map of the area
animal.map <- geom_polygon(aes(x = long, y = lat, group = group), colour = "white", fill = "grey70", data = states.animal)
#satelite map
library(RgoogleMaps)
library(rgdal)
source("code/ggooglemaps.R")
source("code/map-bounding.r")

# get satellite image for the affected area
map_center <- c(lon=-88.875, lat=25.50)
sat_map <-ggooglemap(center = map_center, zoom = 6, maptype='hybrid', n_pix=640)
# location and tag for oil rig
rig <- data.frame(Longitude = -88.365997, Latitude = 28.736628)
plot_rig_w <- geom_text(aes(x=Longitude, y=Latitude, label = "x BP Oil Rig"), colour = "white", size = 3, data=rig)
plot_rig_b <- geom_text(aes(x=Longitude, y=Latitude, label = "x"), size = 3.5, data=rig)
plot_rig_b2 <- geom_text(aes(x=Longitude, y=Latitude, label = "BP Oil Rig"), hjust = -.3, size = 3.5, data=rig)

######################## plots ############################

#SALINITY LEVELS OVER TIME RELATED TO LOW SALINITIES ON MAP
# discretize time into intervals

ggplot(salinity, aes(x = Date_Time, y = Salinity, colour = as.integer(as.Date(Date_Time)))) + geom_point() + scale_colour_gradient(low="orange", high="brown") + opts(title = "Salinity Levels Over Time", legend.position = "none") + labs(x = "Date")
ggsave("images/salinity-time.png")

ggplot() + geom_point(aes(x = Longitude, y = Latitude, colour = as.integer(as.Date(Date_Time))), subset(salinity, Salinity <=34)) + animal.map + scale_colour_gradient(low="orange", high="brown")+ plot_rig_b + plot_rig_b2 + opts(title = "Low Salinity Levels (Salinity < 34) Near Rig", legend.position = "none") 
ggsave("images/salinity-map.png")

#PLOT OF BOATS, FLOATS, AND GLIDERS PATHS ON MAP
cbbColourPalette <- scale_colour_manual(values = c("#FFCC00", "#FF3300", "#990066"))
ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
   scale_fill_identity(legend = FALSE) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
  coord_equal() + theme_nothing() + 
   xlim(c(-96.25, -81.5)) + ylim(c(20,31)) + geom_point(aes(x = Longitude, y = Latitude, colour = Type), data = salinity)+ cbbColourPalette+ labs(x = "Longitude", y = "Latitude")+ plot_rig_w 
ggsave("images/boats-floats-gliders.png")

#DEPTH X SALINITY GROUP = INTERACTION(LONG, LAT) FACTTED BY TYPE (takes realllllly long time to load!!!)
bfg.3 <- ggplot() + geom_line(aes(x = Depth, y = Salinity, group = interaction(Longitude, Latitude), colour = Type), data = subset(salinity, Salinity != -99)) + cbbColourPalette + opts(title = "Boats, Floats, and Gliders")
ggsave("images/deapth-salinity.png")


