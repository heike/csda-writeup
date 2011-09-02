#PAH code
#choose lrf_WATER_benchmarks in data folder
setwd("C:\\Users\\Owner\\Documents\\csda-writeup")
surf.water <- read.csv("data/lrf_WATER_benchmarks.csv")
#format data
surf.water$DATE<- as.character(surf.water$DATE)
surf.water$DATE <- as.Date(surf.water$DATE, "%m/%d/%Y")
water.bench1 <- ddply(surf.water, .(LONGITUDE, LATITUDE, SUBSTANCE), summarize,
	avg.Acute.Potency.Ratio = mean(Acute.Potency.Ratio),
	avg.Chronic.Potency.Ratio = mean(Chronic.Potency.Ratio))

water.bench <- ddply(water.bench1, .(LONGITUDE, LATITUDE), summarize,
	Acute.Benchmark.Value = sum(avg.Acute.Potency.Ratio),
	Chronic.Benchmark.Value = sum(avg.Chronic.Potency.Ratio))

#choose lrf_SEDIMENT_benchmarks in data folder
sediment <- read.csv("data/lrf_SEDIMENT_benchmarks.csv")

#choose OC in the data folder
OC <- read.csv("data/OC.csv")
#format data

OC <- ddply(OC, .(LONGITUDE, LATITUDE), transform,
	avgOC = mean(OCRESULT))

OC.sediment <- merge(OC, sediment, by=c("LONGITUDE","LATITUDE")) 
OC.sediment <- subset(OC.sediment, avgOC != 0)

sediment.bench1 <- transform(OC.sediment, OC.Normalized = ((RESULT/avgOC)))
sediment.bench2 <- transform(sediment.bench1, Alkl.Adj.Conc = OC.Normalized*Alkylation.Multiplier)
sediment.bench3 <- transform(sediment.bench2, 
				Acute.Potency.Ratio = Alkl.Adj.Conc/Acute.Potency.Divisor,
				Chronic.Potency.Ratio = Alkl.Adj.Conc/Chronic.Potency.Divisor)
sediment.bench3a <- ddply(sediment.bench3, .(LONGITUDE, LATITUDE, SUBSTANCE), summarize,
				avg.Acute.Potency.Ratio = mean(Acute.Potency.Ratio),
				avg.Chronic.Potency.Ratio = mean(Chronic.Potency.Ratio))
sediment.bench <- ddply(sediment.bench3a, .(LONGITUDE, LATITUDE), summarize,
			Acute.Benchmark.Value = sum(avg.Acute.Potency.Ratio),
			Chronic.Benchmark.Value = sum(avg.Chronic.Potency.Ratio))
sediment.bench3$DATE<- as.character(sediment.bench3$DATE)
sediment.bench3$DATE <- as.Date(sediment.bench3$DATE, "%m/%d/%Y")
sediment.bench3$ratios <- with(sediment.bench3, Chronic.Potency.Ratio/Acute.Potency.Ratio)
sediment.bench4 <-  na.omit(unique(sediment.bench3[,c("SUBSTANCE", "ratios")]))
nsub <- length(unique(surf.water$SUBSTANCE))+1
amax <- max(surf.water$Acute.Potency.Ratio)
cmax <- 1/mean(sediment.bench3$ratios, na.rm=T)
acute <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,amax+.1,amax+.1,1)), fill="red", alpha=0.1)
acute_text <- geom_text(aes(x=(nsub+1)/2, y=mean(c(1,amax)), label="acute"), colour="red", size=15, alpha=0.5)
chronic <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,cmax,cmax,1)), fill="yellow", alpha=0.1)
chronic_line <- geom_hline(aes(yintercept=cmax), colour="grey50")
chronic_data <- data.frame(x=(nsub+1)/2, y=mean(c(1,cmax)), Danger.Level="Carcinogenic")
chronic_text <- geom_text(aes(x,y, label="chronic"), angle=90, colour="yellow", size=15, alpha=0.5, data=chronic_data)
#geom_path(aes(x=as.numeric(SUBSTANCE), y=1/ratios), data=sediment.bench3)

local.data <- surf.water[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")]
local.data <- rbind(local.data, sediment.bench3[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")])
local.data <- subset(local.data, as.character(SUBSTANCE) !="")
local.data$SUBSTANCE <- factor(local.data$SUBSTANCE)
water.danger <- qplot(x = reorder(SUBSTANCE, Acute.Potency.Ratio, mean), y = Acute.Potency.Ratio, size=I(4), data = local.data) + geom_hline(yintercept = 1, colour="grey50") + chronic_line
water.danger <- water.danger  + coord_flip() + labs(y = "Acute Potency Ratio", x = "Substance")  + acute + acute_text + chronic +  chronic_text
water.danger + opts(title = "Chronic and Acute Potency Ratios") 

ggsave("images/chron-acute-ratios.png")
#------------------------------------------------------------------------------------------------------

total <- rbind(water.bench, sediment.bench) 
ggplot() +
 geom_tile(aes(lon, lat, fill=fill), data = sat_map) +
  scale_fill_identity(legend=F) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +  
  coord_equal() + theme_nothing() + 
  plot_rig +xlim(c(-96.25, -81.5)) + ylim(c(22,31)) +
geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "light blue", data = total) + 
geom_point(aes(x = LONGITUDE, y = LATITUDE), size = 3, colour = "yellow", data = subset(total, Chronic.Benchmark.Value >= 1)) +
geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "red", data = subset(total, Acute.Benchmark.Value >= 1)) + 
opts(title = "Polycyclic Aromatic Hydrocarbons: Chronic and Acute Levels") + labs(x = "Longitude", y = "Latitude") 
ggsave("images/chron-acute-map.png")


#timeline
##needs some adjustments - lost previous code :(
sediment.bench3$Log_Acute.Potency.Ratio<-log((sediment.bench3$Acute.Potency.Ratio+1))
surf.water$Log_Acute.Potency.Ratio<-log((surf.water$Acute.Potency.Ratio+1))

ggplot() + geom_vline(yintercept=log(2), colour="grey50") + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(sediment.bench3, RESULT!=0)) + 
scale_colour_discrete() +labs(y="Log of Acute Potency Ratio", x="Date") + ylim(0,0.6) + opts(title="Sediment Acute Potency Ratios") 

ggsave("images/acute-timeline.png")

ggplot() + geom_hline(aes(yintercept=log(2)), colour="black") + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(surf.water, RESULT!=0)) + 
scale_colour_discrete() +labs(y="Log of Acute Potency Ratio", x="Date") + opts(title="Surface Water Acute Potency Ratios") 

ggsave("images/acute-timeline2.png") 

ggplot() + geom_hline(aes(yintercept=log(2)), colour="grey50") + geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(sediment.bench3, RESULT!=0)) +
geom_point(aes(x = DATE, y = Log_Acute.Potency.Ratio, colour=Danger.Level), size =5, data = subset(surf.water, RESULT!=0)) + 
scale_colour_discrete() + geom_text(aes(x = DATE, y = Log_Acute.Potency.Ratio, label = SUBSTANCE, colour = Danger.Level, hjust = -.1), data = subset(surf.water, Log_Acute.Potency.Ratio >= log(2))) +
labs(y="Log of Acute Potency Ratio", x="Date")  + opts(title="Acute Potency Ratios for Surface Water and Sediment") 

ggsave("images/acute-timeline3.png")

# need to still fix height and width
sediment.bench4 <-  na.omit(unique(sediment.bench3[,c("SUBSTANCE", "ratios")]))
nsub <- length(unique(surf.water$SUBSTANCE))+1
amax <- max(surf.water$Acute.Potency.Ratio)
cmax <- 1/mean(sediment.bench3$ratios, na.rm=T)
acute <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,amax+.1,amax+.1,1)), fill="red", alpha=0.1)
acute_text <- geom_text(aes(x=(nsub+1)/2, y=mean(c(1,amax)), label="acute"), colour="red", size=15, alpha=0.5)
chronic <- geom_polygon(aes(x=c(0.5,0.5,nsub,nsub), y=c(1,cmax,cmax,1)), fill="yellow", alpha=0.1)
chronic_line <- geom_hline(aes(yintercept=cmax), colour="grey50")
chronic_data <- data.frame(x=(nsub+1)/2, y=mean(c(1,cmax)), Danger.Level="Carcinogenic")
chronic_text <- geom_text(aes(x,y, label="chronic"), angle=90, colour="yellow", size=15, alpha=0.5, data=chronic_data)
#geom_path(aes(x=as.numeric(SUBSTANCE), y=1/ratios), data=sediment.bench3)

local.data <- surf.water[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")]
local.data <- rbind(local.data, sediment.bench3[,c("SUBSTANCE", "Acute.Potency.Ratio", "Danger.Level")])
local.data <- subset(local.data, as.character(SUBSTANCE) !="")
local.data$SUBSTANCE <- factor(local.data$SUBSTANCE)
water.danger <- qplot(x = reorder(SUBSTANCE, Acute.Potency.Ratio, mean), y = Acute.Potency.Ratio, size=I(4), data = local.data) + geom_hline(yintercept = 1, colour="grey50") + chronic_line
water.danger <- water.danger  + coord_flip() + labs(y = "Acute Potency Ratio", x = "Substance")  + acute + acute_text + chronic +  chronic_text
water.danger + opts(title = "Chronic and Acute Potency Ratios") 
dev.off()
