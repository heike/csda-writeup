library(DBI)
library(RMySQL)
airport <- read.csv(file.choose())
drv <- dbDriver("MySQL")
setwd("C:\\Users\\Owner\\Documents\\csda-writeup")
co <- dbConnect(drv, user="2009Expo", password="R R0cks", port=3306, dbname="data_expo_2009", host="headnode.stat.iastate.edu")

ar <- dbGetQuery(co, "select * from airportsR")
lax <- subset(airport, iata=="LAX")
fai <- subset(airport, iata=="FAI")
phx <- subset(airport, iata=="PHX")
sea <- subset(airport, iata=="SEA")
iwa <- subset(airport, iata=="IWA")
sjc <- subset(airport, iata=="SJC")
fws <- subset(airport, iata=="FWS")
dwh <- subset(airport, iata=="DWH")
gal <- subset(airport, iata=="GAL")
lhv <- subset(airport, iata=="LHV")
ena <- subset(airport, iata=="ENA")
ffz <- subset(airport, iata=="FFZ")
cic <- subset(airport, iata=="CIC")
blv <- subset(airport, iata=="BLV")
glh <- subset(airport, iata=="GLH")
eug <- subset(airport, iata=="EUG")
aus <- subset(airport, iata=="AUS")
sdm <- subset(airport, iata=="SDM")
lvk <- subset(airport, iata=="LVK")
s43 <- subset(airport, iata=="S43")
ith <- subset(airport, iata=="ITH")
smf <- subset(airport, iata=="SMF")
sck <- subset(airport, iata=="SCK")
chd <- subset(airport, iata=="CHD")
hef <- subset(airport, iata=="HEF")
library(ggplot2)
library(RgoogleMaps)
library(rgdal)
source("code/ggooglemaps.R")

map_center <- c(lon=s43$longitude, lat=s43$latitude)
sat_map <-ggooglemap(center = map_center, zoom = 14, maptype='hybrid', n_pix=640)
save(sat_map, file="s43.rds") 
ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
   scale_fill_identity(legend = F) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
   theme_nothing()


