library(DBI)
library(RMySQL)

drv <- dbDriver("MySQL")

co <- dbConnect(drv, user="2009Expo", password="R R0cks", port=3306, dbname="data_expo_2009", host="headnode.stat.iastate.edu")

ar <- dbGetQuery(co, "select * from airportsR")
lax <- subset(ar, iata=="LAX")

library(ggplot2)
library(RgoogleMaps)
library(rgdal)
source("code/ggooglemaps.R")
map_center <- c(lon=lax$longitude, lat=lax$latitude)

sat_map <-ggooglemap(center = map_center, zoom = 13, maptype='hybrid', n_pix=640)

save(sat_map, file="lax.rds")
ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
   scale_fill_identity(legend = F) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
   theme_nothing()