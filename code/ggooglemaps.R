

# from David Kahle's 2010 ggplot2 casestudy contribution
# slightly modified to the purposes of loading larger scale maps
# and adjusted to the newer version of the Google Maps API
library(ggplot2)
library(ReadImages)
library(RgoogleMaps)
library(MASS)
theme_set(theme_bw())


################################################################################
#################### preload functions for later use        ####################
################################################################################

ggimage <- function(image){
  require(ggplot2)
	
  if(length(dim(image)) == 2){
    message('creating black and white image...')
    image <- melt(image)
    names(image) <- c('row','column','fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_gradient(low = 'black', high = 'white')
  }
  
  if(length(dim(image)) == 3){
  	message('creating color image...')
  	image <- apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
    image <- melt(image)
    names(image) <- c('row', 'column', 'fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()  	
  }

  #return(plot) # remove first pound for the image in the case study
  plot +
    opts(      
      axis.line = theme_blank(), axis.ticks = theme_blank(),
      axis.text.x = theme_blank(), axis.text.y = theme_blank(), 
      axis.title.x = theme_blank(), axis.title.y = theme_blank(),
      axis.ticks.length = unit(0, "lines"), 
      axis.ticks.margin = unit(0, "lines"),
      legend.position = "none", 
      panel.background = theme_blank(), 
      panel.border = theme_blank(), 
      panel.grid.major = theme_blank(), 
      panel.grid.minor = theme_blank(), 
      panel.margin = unit(0, "lines"), 
      plot.background = theme_blank(), 
      plot.title = theme_blank(), 
      plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
    )
}





ggooglemap <- function(location = 'houston', 
  center = c(lat = 29.7632836, lon = -95.3632715), 
    type = c('color','bw')[1], rgbcoefs = c(0, 1, 0), zoom = 10, 
    maptype = 'terrain',
    destfile = 'TemporaryMap.jpg', n_pix = 700)
{
  require(ggplot2)	
  require(RgoogleMaps)
  require(ReadImages)	
	
  if(!missing(location)){
    url_string <- paste('http://maps.google.com/maps/geo?q=', location, sep = '')
    site   <- readLines(url(url_string))	
    site   <- site[which(regexpr('coordinates', site) > 0)]
    if(is.na(site)) stop('location geocoding error.')
    site   <- strsplit(site, '[')[[1]][2]
    site   <- strsplit(site, ',')[[1]][1:2]
    latlon <- as.numeric(site)	
    center <- c(lat = latlon[2], lon = latlon[1])
    closeAllConnections()
  }
	
#  if(missing(API)) API <- 'ABQIAAAAjXgA9fcDJ5x9jxpe1zJlbRQJq1A3v58B8-5UEIEN_9ylyBucrxQ0gU-cz0HZ0nHaJEkljUCmmzAMmg'
    
  # get map
  GetMap(center = center[c('lat','lon')], 
    size = c(n_pix, n_pix), zoom = zoom, format = 'jpg', 
    maptype = maptype, destfile = destfile)
    
  # load map  
  map <- read.jpeg(destfile)
  
  # deal with color
  if(type == 'color'){
    map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))     
  } else if(type == 'bw') {
  	nrow <- nrow(map)
  	ncol <- ncol(map)  	
    map <- grey(rgb2grey(map, coefs = rgbcoefs))
    map <- matrix(map, nrow = nrow, ncol = ncol)
  } else {
    stop('type must be either 661b56e4002b777f1f17cebd9787e84ff72f6537#39;color\' or 661b56e4002b777f1f17cebd9787e84ff72f6537#39;bw\'', call. = FALSE)
  }
  
  # reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center['lat'], center['lon'])
  #XY2LatLon(HouMapInfo, XY_cent$newX, XY_cent$newY)  
  
  # geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  # merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = 'right'))
  df <- df[,c('lon','lat','fill')]
  df
}


theme_nothing <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), axis.ticks.margin = unit(0, "lines"), 
    panel.background = theme_rect(fill = 'white'), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_rect(colour = 'white'), 
    plot.title = theme_text(size = base_size * 1.2), 
    plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
  ), class = "options")
}

# without the white lines - still has faint light lines
theme_nothing <- function (base_size = 12){
  structure(list(
panel.grid.minor=theme_blank(), 
panel.grid.major=theme_blank(), 
panel.background=theme_blank(), 
panel.border = theme_blank(), 
panel.margin = unit(0, "lines"), 
axis.title.x=theme_blank(), axis.title.y=theme_blank(), 
axis.line=theme_blank(), 
axis.ticks=theme_blank(), 
axis.text.y = theme_text(colour="#FFFFFF"), 
axis.text.x = theme_text(colour = "#FFFFFF"),
legend.position = "none"
), class = "options")
}






vplayout <- function(x, y)  viewport(layout.pos.row = x, layout.pos.col = y) 