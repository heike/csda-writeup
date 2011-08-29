# Figure 1: 
# Annotated Time Line:


library(ggplot2)
airport.new<-read.csv("data/airports-volume.csv")
airatl<-subset(airport.new, Dest=='ATL')
airstl<-subset(airport.new, Dest=='STL')
airord<-subset(airport.new, Dest=='ORD')
airsea<-subset(airport.new, Dest=='SEA')
air<-as.data.frame(rbind(airstl,airsea,airatl,airord))
air$Time<-air$Year+air$Month/12
air$Volume<-air$Freq/air$NDays
air$Airport <- factor(air$Dest)

line1<-data.frame(x=c(1989, 1989.25, 1991, 1991.085, 2001, 2001.085, 2001, 2001.75, 2003.01, 2003.085, 2004, 2003.92),
                  xend=c(1989.25, 1989.25, 1991.085, 1991.085, 2001.085, 2001.085, 2001.75, 2001.75, 2003.085, 2003.085, 2003.92, 2003.92),
                  y=c(900, 900, 1300, 1300, 900, 900, 1300, 1300, 1300, 1300, 600, 600),
                  yend=c(900,0, 1300, 0, 900, 0, 1300, 0, 1300, 0, 600, 0))
p <- qplot(Time, Volume, data= air, geom="point", colour=I(rgb(0,0,0,0.1)), size=I(1), main="Volume at Major Airports: Atlanta, Chicago, Seattle, St Louis", ylim=c(0,1500), xlim=c(1985,2010))
p <- p + geom_segment(data=line1, mapping=aes(x=x,xend=xend, y=y, yend=yend), colour=I(rgb(0,0,0,.25)), size=I(0.5))
p <- p + annotate("text", x=1989, y=970, label=c("March '89 - Eastern Airline"),
colour="grey50", size=3, hjust=1)+
annotate("text", x=1989, y=900, label=c("workers go on STRIKE"),
colour="grey50", size=3, hjust=1)+
annotate("text", x=1991, y=1300, label=c("Feb '91 - Eastern Airline"),
colour="grey50", size=3, hjust=1)+
annotate("text", x=1991, y=1230, label=c("ceases all operations"),
colour="grey50", size=3, hjust=1)+
annotate("text", x=2001.085, y=900, label=c("? "), colour="grey50",
size=3, hjust=1)+
annotate("text", x=2001, y=1300, label=c("Sep '01 - W.T.C. Attack"),
colour="grey50", size=3, hjust=1)+
annotate("text", x=2003.1, y=1370, label=c("Mar 03' - Several carriers expand"), colour="grey50", size=3, hjust=0)+
annotate("text", x=2003.1, y=1300, label=c("their operations in Atlanta"),
colour="grey50", size=3, hjust=0)+
annotate("text", x=2004, y=670, label=c("Nov 03' - A.A. Buys T.W.A. and "),
colour="grey50", size=3, hjust=0)+
annotate("text", x=2004, y=600, label=c("cuts flights in St. Louis"),
colour="grey50", size=3, hjust=0)
p <- p + geom_line(data=air, mapping=aes(x=Time, y=Volume, colour=Airport, group=Airport), size=I(1))
p
ggsave(file="airports.pdf",width=10, height=4)

#######################
# Figure 2: 

library(RMySQL)
m <- dbDriver("MySQL")
co <- dbConnect(m, user="2009Expo", password="R R0cks", port=3306, dbname="data_expo_2009", host="headnode.stat.iastate.edu")
dtime <- dbGetQuery(co, "select Year, avg(CRSArrTime), avg(ArrTime), count(*) as count,avg(ArrDelay) from ontime group by Year, (CRSArrTime div 100)")

qplot(`avg(CRSArrTime)`, `avg(ArrDelay)`, geom="point", data=subset(dtime, Year > 1998), xlab="Scheduled Arrival", ylab="Average Arrival Delay (in mins)") + facet_wrap(facets=~Year, ncol=5)+geom_hline(yintercept=c(0,15))


#######################
# Figure 3: Maps of ghost flights

airports <- read.csv("data/airports.csv")


airport.location <- function(iata) {
	x <- as.vector(t(airports[airports$iata==iata,c(7:6)]))
	if (length(x)==0) x <- c(NA,NA)
	return (x)
}


ghosts <- read.csv("data/ghosts.csv", as.is=T)
ghosts$first <- with(ghosts, pmin(Dest, Dest.1))
ghosts$second <- with(ghosts, pmax(Dest, Dest.1))

dt <- as.data.frame(xtabs(~first+second+Year+UniqueCarrier, subset(ghosts, UniqueCarrier %in% c("B6","FL","HA"))))
dt <- dt[-which(dt$Freq==0),]


dt2 <- merge(dt, airports[,c("iata", "latitude", "longitude")], by.x="first", by.y="iata", all.x=TRUE)
names(dt2)[6:7] <- c("y1", "x1")
dt.all <- merge(dt2, airports[,c("iata", "latitude", "longitude")], by.x="second", by.y="iata", all.x=TRUE)

names(dt.all)[8:9] <- c("y2", "x2")

head(dt.all)
dt.all$ID <- 1:nrow(dt.all)
dt.melt <- melt(dt.all, id.vars=c("ID", "Year", "UniqueCarrier", "Freq"), measure.vars = 6:9)


dt.melt$Direction <- "To"
dt.melt$Direction[grep("1", dt.melt$variable)] <- "From"
dt.melt$Loc <- "Y"
dt.melt$Loc[grep("x", dt.melt$variable)] <- "X"

dt.cast <- cast(dt.melt, ID+Year+UniqueCarrier+Freq+Direction ~ Loc)


require(maps)
states <- map_data("state")
airports.more <- read.csv("data/airport-data.csv")

map.opts <- opts(panel.grid.minor=theme_blank(), 
	  panel.grid.major=theme_blank(),
	  panel.background=theme_blank(),
	  axis.title.x=theme_blank(),
	  axis.title.y=theme_blank(),
	  axis.line=theme_blank(),
	  axis.ticks=theme_blank(),
	  axis.text.y  = theme_text(colour="#FFFFFF"),
	  axis.text.x = theme_text(colour = "#FFFFFF"))

usamap <- ggplot() + geom_polygon(aes(x=long, y=lat, group=group), fill="grey85", colour="white", data=states) + map.opts


usamap + geom_line(aes(x=X,y=Y, group=ID, size=I(log10(Freq)/3), weight=Freq), colour=rgb(216/255, 70/255, 202/255,0.6), data=subset(dt.cast, (Year==2008) & (UniqueCarrier %in% c("HA")))) + geom_point(aes(x=longitude, y=latitude), size=0.7, colour="grey25", data=subset(airports.more, (Volume>100) & (latitude > 20) & (latitude < 50) & (longitude > -169) & (longitude < -65)))  +
opts(legend.position="none") 

usamap + geom_line(aes(x=X,y=Y, group=ID, size=I(log10(Freq)/3), weight=Freq), colour=rgb(216/255, 70/255, 202/255,0.6), data=subset(dt.cast, (Year==2008) & (UniqueCarrier %in% c("B6")))) + geom_point(aes(x=longitude, y=latitude), size=0.7, colour="grey25", data=subset(airports.more, (Volume>100) & (latitude > 20) & (latitude < 50) & (longitude > -169) & (longitude < -65)))  +
opts(legend.position="none") 

usamap + geom_line(aes(x=X,y=Y, group=ID, size=I(log10(Freq)/3), weight=Freq), colour=rgb(216/255, 70/255, 202/255,0.6), data=subset(dt.cast, (Year==2008) & (UniqueCarrier %in% c("FL")))) + geom_point(aes(x=longitude, y=latitude), size=0.7, colour="grey25", data=subset(airports.more, (Volume>100) & (latitude > 20) & (latitude < 50) & (longitude > -169) & (longitude < -65)))  +
opts(legend.position="none") 

#######################
# Figure 4: Polar boxplots of wind direction in line-up plot


# PHX has three runways going in the same direction
library("DBI")
library("RMySQL")
library(ggplot2)
m <- dbDriver("MySQL")
con <- dbConnect(m, user="2009Expo", password="R R0cks", port=3306,
dbname="data_expo_2009", host="headnode.stat.iastate.edu")

WindEffects <- function (AirportID, N=20, col='orange'){
	ARPT.flgt <- dbGetQuery(con, paste("select Year, Month, DayofMonth, ArrTime div  
	100, avg(ArrDelay) ,count(*) from ontime where Dest='", AirportID, "' group by Year, Month, DayofMonth, ArrTime div 100", sep=''))
	print(dim(ARPT.flgt))

	ARPT.weather <- dbGetQuery(con, paste("select * from weather where IATA='", AirportID, "' and Year>=1991", sep=''))
	print(dim(ARPT.weather))
	summary(ARPT.weather$Wind_SpeedMPH)

	ARPT.weather[ARPT.weather[,"Wind_Direction"]=='North',"Wind_Direction"] <- 'N' 
	ARPT.weather[ARPT.weather[,"Wind_Direction"]=='South',"Wind_Direction"] <- 'S' 
	ARPT.weather[ARPT.weather[,"Wind_Direction"]=='East',"Wind_Direction"] <- 'E' 
	ARPT.weather[ARPT.weather[,"Wind_Direction"]=='West',"Wind_Direction"] <- 'W' 

	wind.dir <- c("N","NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",  
	"S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

	ARPT.wind <- transform(ARPT.weather,
	     Wind_Direction=factor(Wind_Direction, levels=c("Calm",  
	"Variable", wind.dir)),
		Events=factor(Events),
		Conditions=factor(Conditions),
		Station=factor(Station),
		Thunderstorm=factor(Thunderstorm),
		Rain=factor(Rain),
		Snow=factor(Snow),
		Fog=factor(Fog),
		Hail=factor(Hail)
	)

	ARPT.wind$Hour2<-ARPT.wind$Hour%/%100
	colnames(ARPT.flgt)[4]<-"Hour2"
	colnames(ARPT.flgt)[3]<-"Day"
	colnames(ARPT.flgt)[5]<-"ArrDelay"
	colnames(ARPT.flgt)[6]<-"N"

	ARPT.flgt.wind<-merge(ARPT.flgt,ARPT.wind,by=c("Year","Month","Day","Hour2"))
	ARPT.flgt.wind[1,]

	## This next set of code is used to infer differences in the samples
		ARPT.flgt.wind2<-subset(ARPT.flgt.wind,Wind_SpeedMPH>20)
		ARPT.flgt.wind2<-subset(ARPT.flgt.wind2,ArrDelay>=0)
		ARPT.flgt.wind2$Wind_Direction <- factor(ARPT.flgt.wind2$Wind_Direction, levels=wind.dir)
		ARPT.flgt.wind.inference<-ARPT.flgt.wind2
		true.plot.group<- sample(1:N,1)
		ARPT.flgt.wind.inference$Group<-true.plot.group

		for(i in c(1:N)[-true.plot.group]){
			ARPT.flgt.wind2$Wind_Direction <- ARPT.flgt.wind2$Wind_Direction[sample(dim(ARPT.flgt.wind2)[1],dim(ARPT.flgt.wind2)[1], replace=FALSE)]
			ARPT.flgt.wind2$Group<-i
			ARPT.flgt.wind.inference <- rbind(ARPT.flgt.wind.inference, ARPT.flgt.wind2)
		}

		p6.1.2 <- qplot (Wind_Direction, ArrDelay, data=ARPT.flgt.wind.inference,
 		       geom=c("boxplot"), 
 		       ylim=c(-20,100)) + xlim(c(wind.dir,"N")) + coord_polar(start=0) +
 		       facet_wrap( ~ Group) + ylab("Arrival Delay (in minutes)") + xlab("Wind Direction") 
 		       
 	print(true.plot.group)
	## The data for inference is certainly created above.  However, the plot types that best describe the trend need to be accentuated
	return(p6.1.2)
}

AirportID <- 'PHX'
pdf(file=paste(AirportID, "Inferenceouput.pdf", sep=''), width=18, height=17)
print(WindEffects(AirportID, N=10))
dev.off()

