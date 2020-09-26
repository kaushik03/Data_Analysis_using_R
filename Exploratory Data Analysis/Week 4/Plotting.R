# Answersing Questions

# Library loaded

library(ggplot2)
library(plyr)

# Tidying Data for convinience.
# -------------------------------------------------------------------------
## Converting columns below  to factor
ToFactor <- c("year", "type", "Pollutant","SCC","fips")
NEI[,ToFactor] <- lapply(NEI[,ToFactor], factor)

head(levels(NEI$fips))

## The levels have NA as "   NA", so converting that level back to NA
levels(NEI$fips)[1] = NA
NEIdata<-NEI[complete.cases(NEI),]
colSums(is.na(NEIdata))

# Question 1
# -------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# --------------------------------------------------------------------------

totalEmission <- aggregate(Emissions ~ year, NEIdata, sum)
totalEmission

# Plotting as a bar plot 


barplot(
  (totalEmission$Emissions)/10^6,
  names.arg=totalEmission$year,
  xlab="Year",
  ylab="PM2.5 Emissions in Tons",
  main="Total PM2.5 Emissions From All US Sources"
)

# Question 2
# -----------------------------------------------------------------------------
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
# --------------------------------------------------------------------------

NEIdataBaltimore<-subset(NEIdata, fips == "24510")
totalEmissionBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)
totalEmissionBaltimore

barplot(
  (totalEmissionBaltimore$Emissions)/10^6,
  names.arg=totalEmissionBaltimore$year,
  xlab="Year",
  ylab="PM2.5 Emissions in Tons",
  main="Total PM2.5 Emitted in Baltimore City"
)

# Question 3
# --------------------------------------------------------------------------
# Of the four types of sources indicated by the type (point, nonpoint,
# onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
# --------------------------------------------------------------------------


g<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIdataBaltimore)
g+geom_bar(stat="identity")+
  facet_grid(.~type)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by 
                        Source Type"))+
  guides(fill=FALSE)



# Question 4
# --------------------------------------------------------------------------
# Across the United States,
# how have emissions from coal combustion-related sources changed from 1999-2008?
# --------------------------------------------------------------------------

names(SCC)<-gsub("\\.","", names(SCC))

SCCcombustion<-grepl(pattern = "comb", SCC$SCCLevelOne, ignore.case = TRUE)
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)

## extracting the SCC in 
SCCCoalCombustionSCC<-SCC[SCCcombustion & SCCCoal,]$SCC
NIECoalCombustionValues<-NEIdata[NEIdata$SCC %in% SCCCoalCombustionSCC,]
NIECoalCombustionTotalEm<-aggregate(Emissions~year, NIECoalCombustionValues, sum)

g<-ggplot(aes(year, Emissions/10^5), data=NIECoalCombustionTotalEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +
  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US 
                        from 1999-2008"))



# Question 5
# --------------------------------------------------------------------------
# How have emissions from motor vehicle sources changed from 1999-2008 
# in Baltimore City?
# --------------------------------------------------------------------------

CCvehicle<-grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC

## using this boolean vector get the interested rows from the baltimore data
NEIvehicleSSC <- NEIdata[NEIdata$SCC %in% SCCvehicleSCC, ]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)


g<-ggplot(aes(year, Emissions/10^5), data=NIEvehicleBaltimoreTotEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +
  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore 
                        from 1999-2008"))


# Question 6
# --------------------------------------------------------------------------
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?
# --------------------------------------------------------------------------

NEIvehicleBalti<-subset(NEIvehicleSSC, fips == "24510")
NEIvehicleBalti$city <- "Baltimore City"
NEIvehiclela<-subset(NEIvehicleSSC, fips == "06037")
NEIvehiclela$city <- "Los Angeles County"
NEIBothCity <- rbind(NEIvehicleBalti, NEIvehiclela)

ggplot(NEIBothCity, aes(x=year, y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(.~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))