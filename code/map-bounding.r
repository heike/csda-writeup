library(ggplot2)
library(maps)

getbox <- function (map, xlim, ylim) {
	# identify all regions involved
	small <- subset(map, (long > xlim[1]) & (long < xlim[2]) & (lat > ylim[1]) & (lat < ylim[2]))
	regions <- unique(small$region)
	small <- subset(map, region %in% regions)	

	# now shrink all nodes back to the bounding box
	small$long <- pmax(small$long, xlim[1])
	small$long <- pmin(small$long, xlim[2])
	small$lat <- pmax(small$lat, ylim[1])
	small$lat <- pmin(small$lat, ylim[2])

	return(small)
}

## use it
states <- map_data("state")
states.small <- getbox(states, c(-100,-80), c(20, 32))

ggplot() + geom_polygon(aes(x=long, y=lat, group=group), data=states.small)

world <- map_data("world")
nasa <- getbox(world, xlim = c(-114.93, -55.07), ylim = c(-21.53, 36.64))

