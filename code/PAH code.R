#PAH code
#choose lrf_WATER_benchmarks in data folder
setwd("C:\\Users\\Owner\\Documents\\csda-writeup")

################# Libraries Used #################
library(ggplot2)

################# Read in Data #################
surfwater <- read.csv("data/lrf_WATER_benchmarks.csv")

#choose lrf_SEDIMENT_benchmarks in data folder
sediment <- read.csv("data/lrf_SEDIMENT_benchmarks.csv")

#choose OC in the data folder
OC <- read.csv("data/OC.csv")

################# Format Data #################
surfwater$DATE <- as.Date(as.character(surfwater$DATE), "%m/%d/%Y")

# at each location summarize each substance
waterbench_substance <- ddply(surfwater, .(LONGITUDE, LATITUDE, SUBSTANCE), summarize,
	avg.Acute.Potency.Ratio = mean(Acute.Potency.Ratio),
	avg.Chronic.Potency.Ratio = mean(Chronic.Potency.Ratio))

# get overall benchmark values at each location
waterbench <- ddply(waterbench_substance, .(LONGITUDE, LATITUDE), summarize,
	Acute.Benchmark.Value = sum(avg.Acute.Potency.Ratio),
	Chronic.Benchmark.Value = sum(avg.Chronic.Potency.Ratio))

##map##
states <- map_data("state")
states.chem <- getbox(states, xlim=c(-96, -82), ylim=c(28,35))
chem.map <- geom_polygon(aes(x = long, y = lat, group = group), colour = "white", fill = "grey70", data = states.chem)

#format data
#### what does OC stand for? ####
#### what do these percentage values mean? usually we can't average over percentages ####
#### there are either one or two measurements ####
#### some of the measurements are in the same place, but at two different time points ####

OC <- ddply(OC, .(LONGITUDE, LATITUDE), transform,
	 avgOC = mean(OCRESULT))
	 
OC <- ddply(OC, .(LONGITUDE, LATITUDE), transform,
	  n= length(OCRESULT))

OCsediment <- merge(OC, sediment, by=c("LONGITUDE","LATITUDE")) 

OCsediment$OCNormalized <- with(OCsediment, RESULT/avgOC)
OCsediment$AlklAdjConc <- with(OCsediment, OCNormalized*Alkylation.Multiplier)

OCsediment$Acute.Potency.Ratio <- with(OCsediment, AlklAdjConc/Acute.Potency.Divisor)
OCsediment$Chronic.Potency.Ratio <- with(OCsediment, AlklAdjConc/Chronic.Potency.Divisor)


sediment.bench3 <- transform(OCsediment, 
				Acute.Potency.Ratio = AlklAdjConc/Acute.Potency.Divisor,
				Chronic.Potency.Ratio = AlklAdjConc/Chronic.Potency.Divisor)
sediment.bench3a <- ddply(sediment.bench3, .(LONGITUDE, LATITUDE, SUBSTANCE), summarize,
				avg.Acute.Potency.Ratio = mean(Acute.Potency.Ratio),
				avg.Chronic.Potency.Ratio = mean(Chronic.Potency.Ratio))
sediment.bench <- ddply(sediment.bench3a, .(LONGITUDE, LATITUDE), summarize,
			Acute.Benchmark.Value = sum(avg.Acute.Potency.Ratio),
			Chronic.Benchmark.Value = sum(avg.Chronic.Potency.Ratio))

sediment.bench3$DATE <- as.Date(as.character(sediment.bench3$DATE), "%m/%d/%Y")
sediment.bench3$ratios <- with(sediment.bench3, Chronic.Potency.Ratio/Acute.Potency.Ratio)
sediment.bench4 <-  na.omit(unique(sediment.bench3[,c("SUBSTANCE", "ratios")]))
nsub <- length(unique(surfwater$SUBSTANCE))+1
amax <- max(surfwater$Acute.Potency.Ratio)
cmax <- 1/mean(sediment.bench3$ratios, na.rm=T)


acute <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,amax+.1,amax+.1,1)), fill="red", alpha=0.1)
acute_text <- geom_text(aes(x=(nsub+1)/2, y=mean(c(1,amax)), label="acute"), colour="red", size=15, alpha=0.5)
chronic <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,cmax,cmax,1)), fill="yellow", alpha=0.1)
chronic_line <- geom_hline(aes(yintercept=cmax), colour="grey50")
chronic_data <- data.frame(x=(nsub+1)/2, y=mean(c(1,cmax)), Danger.Level="Carcinogenic")
chronic_text <- geom_text(aes(x,y, label="chronic"), angle=90, colour="yellow", size=15, alpha=0.5, data=chronic_data)
#geom_path(aes(x=as.numeric(SUBSTANCE), y=1/ratios), data=sediment.bench3)

local.data <- surfwater[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")]
local.data <- rbind(local.data, sediment.bench3[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")])
local.data <- subset(local.data, as.character(SUBSTANCE) !="")
local.data$SUBSTANCE <- factor(local.data$SUBSTANCE)
water.danger <- qplot(x = reorder(SUBSTANCE, Acute.Potency.Ratio, mean), y = Acute.Potency.Ratio, size=I(4), data = local.data) + geom_hline(yintercept = 1, colour="grey50") + chronic_line
water.danger <- water.danger  + coord_flip() + labs(y = "Acute Potency Ratio", x = "Substance")  + acute + acute_text + chronic +  chronic_text
water.danger + opts(title = "Chronic and Acute Potency Ratios") 

ggsave("images/chron-acute-ratios.png")
#------------------------------------------------------------------------------------------------------

total <- rbind(waterbench, sediment.bench) 
d1 <- total
d1$id <- "d1"
d2 <- subset(total, Chronic.Benchmark.Value >= 1)
d2$id <- "d2"
d3 <- subset(total, Acute.Benchmark.Value >= 1)
d3$id <- "d3"
data <- rbind(d1,d2,d3)

ggplot()+
  plot_rig + xlim(c(-96.25, -81.5)) + ylim(c(26,32)) + chem.map +
geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = id),size = 2, data= data) +
theme_grey() +labs(x = "Longitude", y = "Latitude") + 
scale_colour_manual("Level", values = c("d1" = "#FFCC00", "d2" = "#FF6600", "d3" = "#990000"),breaks = c("d1", "d2", "d3"), labels = c("Samples", "Chronic", "Acute")) +
opts(legend.direction = "vertical", legend.position = "right") 
ggsave("images/chron-acute-map.png")


#timeline
##needs some adjustments - lost previous code :(
sediment.bench3$Log_Acute.Potency.Ratio<-log((sediment.bench3$Acute.Potency.Ratio+1))
surfwater$Log_Acute.Potency.Ratio<-log((surfwater$Acute.Potency.Ratio+1))

sediment.time <- ggplot() + geom_vline(yintercept=log(2), colour="grey50") + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(sediment.bench3, RESULT!=0)) + 
scale_colour_discrete() +labs(y="Log of Acute Potency Ratio", x="Date") + ylim(0,0.6) + opts(title="Sediment Acute Potency Ratios") 

water.time <- ggplot() + geom_vline(yintercept=log(2), colour="grey50") + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(surfwater, RESULT!=0)) + 
scale_colour_discrete() +labs(y="Log of Acute Potency Ratio", x="Date") + ylim(0,0.6) + opts(title="Surface Water Acute Potency Ratios") 

ggplot() + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(sediment.bench3, RESULT!=0)) +
geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(surfwater, RESULT!=0)) +
theme_grey() + color_scale_2 + labs(y = "Log of Acute Potency Ratio", x = "Date") + geom_hline(yintercept = log(2), colour = "grey50") +
geom_text(aes(x = DATE, y = Log_Acute.Potency.Ratio, label = SUBSTANCE, colour = Danger.Level), hjust = -.1, data = subset(surfwater, Log_Acute.Potency.Ratio >= log(2)))

#grid.arrange(sediment.time, water.time, ncol = 1)
ggsave("images/acute-timeline.png")

# need to still fix height and width
sediment.bench4 <-  na.omit(unique(sediment.bench3[,c("SUBSTANCE", "ratios")]))
nsub <- length(unique(surfwater$SUBSTANCE))+1
amax <- max(surfwater$Acute.Potency.Ratio)
cmax <- 1/mean(sediment.bench3$ratios, na.rm=T)
acute <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,amax+.1,amax+.1,1)), fill="red", alpha=0.1)
acute_text <- geom_text(aes(x=(nsub+1)/2, y=mean(c(1,amax)), label="acute"), colour="red", size=15, alpha=0.5)
chronic <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,cmax,cmax,1)), fill="yellow", alpha=0.1)
chronic_line <- geom_hline(aes(yintercept=cmax), colour="grey50")
chronic_data <- data.frame(x=(nsub+1)/2, y=mean(c(1,cmax)), Danger.Level="Carcinogenic")
chronic_text <- geom_text(aes(x,y, label="chronic"), angle=90, colour="yellow", size=15, alpha=0.5, data=chronic_data)
#geom_path(aes(x=as.numeric(SUBSTANCE), y=1/ratios), data=sediment.bench3)

local.data <- surfwater[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")]
local.data <- rbind(local.data, sediment.bench3[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")])
local.data <- subset(local.data, as.character(SUBSTANCE) !="")
local.data$SUBSTANCE <- factor(local.data$SUBSTANCE)
water.danger <- qplot(x = reorder(SUBSTANCE, Acute.Potency.Ratio, mean), y = Acute.Potency.Ratio, size=I(4), data = local.data) + geom_hline(yintercept = 1, colour="grey50") + chronic_line
water.danger <- water.danger  + coord_flip() + labs(y = "Acute Potency Ratio", x = "Substance")  + acute + acute_text + chronic +  chronic_text
water.danger + opts(title = "Chronic and Acute Potency Ratios") 
dev.off()
