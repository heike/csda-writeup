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
map_center <- c(lon=-89, lat=25.50)
sat_map <-ggooglemap(center = map_center, zoom = 6, maptype='hybrid', n_pix=640)

# get state outlines for the affected area
library(maps)
states <- map_data("state")
states.animal <- getbox(states, xlim=c(-97, max(animal$Longitude+2)), ylim=range(animal$Latitude)+c(0,1.25))

#colors
color_scale_3 <- scale_colour_manual(values = c("#FF9900", "#CCFF00", "#FF3300"))
color_scale_2 <-  scale_colour_manual(values = c( "#CC0000", "#000099")) 
#---------------------------------------------------------------------------------------------------------
# setting up plot layers

# state map of the area
animal.map <- geom_polygon(aes(x = long, y = lat, group = group), colour = "white", fill = "grey70", data = states.animal)

states.text <- ddply(states.animal, .(region), summarize,
x=mean(long, na.rm=T),
y=mean(lat, na.rm=T))
states.text <- states.text[-6,]
map.text <- geom_text(aes(x=x+c(0,2,-0.5,-0.6,.5,.75),
						  y=c(31.5, 27.3, 31.5, 30.7, 31.25, 30.7),
						  label=region), size=3, colour="white", data=states.text, aes.inherit=F)

# add location and tag for oil rig
rig <- data.frame(Longitude = -88.365997, Latitude = 28.736628)
plot_rig <- geom_text(aes(x=Longitude, y=Latitude, label = "x BP Oil Rig"), colour = "white", size = 3, data=rig)

#---------------------------------------------------------------------------------------------------------
# PLOT OF ALL DEAD ANIMALS ON MAP; BIRDS, SEA TURTLES, WHALES AND DOLPHINS
dead <- subset(animal, Alive=="N")

par(mar=c(0,0,0,0))
plot(rnorm(10), rnorm(10))

ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
   scale_fill_identity(legend = F) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
   theme_nothing() + 
#  xlim(c(-96.25, -81.5)) + ylim(c(20,31)) + 
  geom_point(aes(x = Longitude, y = Latitude, colour = class), data = dead) + 
  coord_cartesian(xlim=range(sat_map$lon), ylim=range(sat_map$lat)) + 
	opts(legend.direction = "horizontal", 
	     legend.position = "bottom", 
	     panel.background =theme_blank(),
	     plot.margin = unit(c(0,0,0,0), "lines"),
	     axis.ticks.margin=unit(0,"lines"), 
	     axis.ticks.length = unit(0,"lines"),
	     panel.margin = unit(0, "lines"),
	     axis.text.y=theme_blank(),
	     axis.text.x=theme_blank()) + plot_rig + color_scale_3 

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

caretta <- ggplot(subset(turtles, Species == "Caretta caretta"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + map.text+ theme_bw() + turtle_opts+ geom_point() + facet_wrap(~Quarter, ncol = 4) + labs(y = "Loggerhead Sea Turtle") + color_scale_2 
chelonia <- ggplot(subset(turtles, Species == "Chelonia mydas"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + map.text + theme_bw() + turtle_opts + geom_point() + facet_wrap(~Quarter, ncol = 4)  +labs(y = "Green Sea Turtle") + color_scale_2
eretmochelys <- ggplot(subset(turtles, Species == "Eretmochelys imbricata"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + map.text + theme_bw() + turtle_opts+ geom_point() + facet_wrap(~Quarter, ncol = 4, drop = FALSE)  + labs(y = "Hawksbill Sea Turtle") + color_scale_2
lepid <- ggplot(subset(turtles, Species == "Lepidochelys kempii"), aes(x = Longitude, y = Latitude, colour = Alive)) + animal.map + map.text + theme_bw() + turtle_opts+ geom_point() + facet_wrap(~Quarter, ncol = 4)  + labs(y = "Kemp's Ridley") + color_scale_2


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


#PERCENT DEAD OF EACH CLASS OF ANIMAL
vlines <- geom_vline(xintercept=as.numeric(as.Date(c("2010-09-19", "2010-07-15"))), colour = "grey70")
annotations <- function(y=20) {
geom_text(aes(x = as.numeric(as.Date(c("2010-09-19", "2010-07-15"))),   label = c("Sep 19, 2010\nRelief Well Completed", "July 15, 2010\nLeak Stopped")), y = y, colour="grey50", hjust = -0.1, angle = 0, size=3, inherit.aes=F)
}

ggplot() + vlines + annotations() + geom_line(aes(x= Date, y =perc_dead, colour = class, group=class), size=3, data = animal_sums, aes.inherit=F) + scale_x_date() + theme_grey() + color_scale_3  + scale_size(legend = FALSE) + labs(x="Date", y="Percent") + opts(legend.position = "none")

ggsave("images/death-rates.pdf", width=10, height=5)

#---------------------------------------------------------------------------------------------------------
# EVERY DAY COUNTS OF DEAD ANIMALS, FACETTED BY CLASS
dead.week <- ddply(animal, .(Date, class), summarize,
    total = length(Species) - sum(Live))
ggplot(aes(x=Date, y=total, colour=class), data=dead.week) + vlines  + geom_point(aes(size = 1.75)) +
 scale_x_date() + facet_wrap(~class, scales="free", nrow=1) + theme_grey() + color_scale_3 + opts(legend.position = "none")+ labs(y = "Count") 

ggsave("images/daily-death-counts.pdf", width=15, height=5) # in pixel


