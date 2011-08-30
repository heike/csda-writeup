# Lendie's path
setwd("C:\\Users\\Owner\\Documents\\csda-writeup")

# load data
#load most affected bird family data
affected<- read.csv("data/affected.csv")
# load animal data
animal <- read.csv("data/animals.csv")
animal$Date <- as.Date(as.character(animal$Date), "%Y-%m-%d")


#----------------------------------------------------------------------------
# load libraries and additional code 
# google maps and satellite images as background
# needs packages:
#   RgoogleMaps, png, rgdal
library(ggplot2)
library(RgoogleMaps)
library(rgdal)
source("code/ggooglemaps.R")
source("code/map-bounding.r")

# get satellite image for the affected area
map_center <- c(lon=-88.875, lat=25.50)
sat_map <-ggooglemap(center = map_center, zoom = 6, maptype='hybrid', n_pix=640)

# get state outlines for the affected area
library(maps)
states <- map_data("state")
states.animal <- getbox(states, xlim=c(-97, max(animal$Longitude+2)), ylim=range(animal$Latitude))

#---------------------------------------------------------------------------------------------------------
# setting up plot layers

# state map of the area
animal.map <- geom_polygon(aes(x = long, y = lat, group = group), colour = "white", fill = "grey70", data = states.animal)

# add location and tag for oil rig
rig <- data.frame(Longitude = -88.365997, Latitude = 28.736628)
plot_rig <- geom_text(aes(x=Longitude, y=Latitude, label = "x BP Oil Rig"), colour = "white", size = 3, data=rig)

#---------------------------------------------------------------------------------------------------------
# PLOT OF ALL DEAD ANIMALS ON MAP; BIRDS, SEA TURTLES, WHALES AND DOLPHINS
dead <- subset(animal, Alive=="N")
ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
   scale_fill_identity(legend = F) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
   theme_nothing() + 
  xlim(c(-96.25, -81.5)) + ylim(c(20,31)) + geom_point(aes(x = Longitude, y = Latitude, colour = class), data = dead) + coord_cartesian(xlim=c(-96.25, -81.5), ylim=c(20,31)) + 
	opts(legend.direction = "horizontal", legend.position = "bottom")+ plot_rig 

ggsave("images/animal_deaths.png")

#---------------------------------------------------------------------------------------------------------
#BARCHART OF DEAD ANIMALS BY CLASS
ggplot(aes(x = class, fill = class), data = dead) + geom_bar() + labs(x = "Class", y = "Count") + opts(title = "Dead Animals", legend.position = "none")
ggsave("images/deaths-by-class.png")

#---------------------------------------------------------------------------------------------------------
# MOST AFFECTED BIRD FAMILIES BAR CHART
ggplot() + geom_bar(aes(x = factor(Common, order = T, c("Gulls, Terns, Skimmers, Skuas", "Pelicans", "Boobies, Gannets", "Herons, Bitterns, Eggrets", "Rails, Gallinules, Coots"))),  data = affected)  + labs(x = "family", y = "# dead birds") + coord_flip() + theme_grey() + opts(title = "Most Affected Bird Families", legend.position = "none")
ggsave("images/bird-families.png")
ggplot() + geom_bar(aes(x = factor(Family, order = T, c("Laridae", "Pelecanidae", "Sulidae", "Ardeidae", "Rallidae"))),  data = affected)  + labs(x = "Family", y = "# dead birds") + theme_grey()
#---------------------------------------------------------------------------------------------------------
# BARCHART OF TURTLE SPECIES COLORED BY CONDITION
ggplot(subset(turtles, Species != "Unknown"), aes(x = factor(Species, order = T, c("Lepidochelys kempii", "Chelonia mydas", "Caretta caretta", "Eretmochelys imbricata")), fill=Alive)) + geom_bar() + theme_grey() + opts(title = "Turtles")+ opts(title = "Turtle Dead/Alive Ratio by Species") + labs(y = "Count", x = "Species") + coord_flip()
ggsave("images/turtle-species-condition.png")
#---------------------------------------------------------------------------------------------------------
# load turtle data
turtles <- read.csv("data/turtles.csv")
library(gridExtra)

turtles$obsDate<- as.Date(as.character(turtles$obsDate), "%Y-%m-%d")

turtles$Quarter[(turtles$week.number >= 18) & (turtles$week.number <= 25)] <- "04-26-2010 to 06-20-2010"
turtles$Quarter[(turtles$week.number >= 26) & (turtles$week.number <=31)] <- "06-21-2010 to 08-01-2010"
turtles$Quarter[(turtles$week.number >= 32) & (turtles$week.number <= 38)] <- "08-02-2010 to 09-19-2010"
turtles$Quarter[turtles$week.number > 38] <- "09-20-2010 to 10-18-210"

# MAP, FACETTED BY QUARTER AND SPECIES OF TURTLE, SHOWING DEAD/ALIVE LOCATIONS

turtle_opts <- opts(axis.title.x=theme_blank(), legend.position = "none", panel.margin=0.1, axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank(), plot.margin=unit(c(0,0,0,0),"lines"))

caretta <- ggplot(subset(turtles, Species == "Caretta caretta"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + geom_point() + facet_wrap(~Quarter, ncol = 4) + labs(y = "Caretta Caretta") + scale_colour_brewer(palette="Set1") + turtle_opts
chelonia <- ggplot(subset(turtles, Species == "Chelonia mydas"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + geom_point() + facet_wrap(~Quarter, ncol = 4) + turtle_opts +labs(y = "Chelonia Mydas") + scale_colour_brewer(palette="Set1")
eretmochelys <- ggplot(subset(turtles, Species == "Eretmochelys imbricata"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + geom_point() + facet_wrap(~Quarter, ncol = 4, drop = FALSE) + turtle_opts + labs(y = "Eretmochelys Imbricata")+ scale_colour_brewer(palette="Set1")
lepid <- ggplot(subset(turtles, Species == "Lepidochelys kempii"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + geom_point() + facet_wrap(~Quarter, ncol = 4) + turtle_opts + labs(y = "Lepidochelys Kempii") + scale_colour_brewer(palette="Set1")


png("images/turtles.png", width=960, height=720) # in pixel
grid.arrange(caretta, chelonia, eretmochelys, lepid, ncol = 1) 
dev.off()

#---------------------------------------------------------------------------------------------------------
# PERCENTAGE DEAD OF EACH CLASS BY DATE
animal_f <- ddply(animal, .(class, Date), summarize,
	n= length(class),
	dead = length(class) - sum(Live, na.rm = T)
)

animal_sums <- ddply(animal_f, .(class), summarize, 
	sum_n = cumsum(n[order(Date)]),
	dead = dead,
	sum_dead = cumsum(dead[order(Date)]),
	perc_dead = cumsum(dead[order(Date)])/sum(dead)*100,
	Date = Date[order(Date)]
)
animal_max <- ddply(animal_sums, .(class), summarize,
	max.dead = max(sum_dead),
	max.week = max(Date)
)

alineperc <- geom_line(aes(x= Date, y =perc_dead, colour = class, group=class), size=3, data = animal_sums)
#PERCENT DEAD OF EACH CLASS OF ANIMAL
ggplot() + alineperc + opts(title="Animal Death Rates by Class") + labs(x="Date", y="Percent") 
ggsave("images/death-rates.png")

#---------------------------------------------------------------------------------------------------------
# EVERY DAY COUNTS OF DEAD ANIMALS, FACETTED BY CLASS
dead.week <- ddply(animal, .(Date, class), summarize,
    total = length(Species) - sum(Live))
animal.1 <-geom_point(aes(x=Date, y=total, colour=class), data = dead.week) 
animal.2 <-geom_smooth(aes(x=Date, y=total, colour=class), data=dead.week) 
ggplot()+ animal.1  + facet_wrap(~class, ncol=3, scales="free") + labs(x="Date", y="Count") + opts(title="Animal Deaths", legend.position = "none")
ggsave("images/daily-death-counts.png")

