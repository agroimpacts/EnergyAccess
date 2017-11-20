library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(devtools)
library(roxygen2)
devtools::document()

library(EnergyAccess)
#Read in IPUMS Data
f<-system.file("Data/idhs_00003.csv", package="EnergyAccess")
IPUMS<- read.csv("Data/idhs_00003.csv", stringsAsFactors = FALSE)
head(IPUMS)
#Reassign into boolean values
IPUMS$ELECTRCHH[which(IPUMS$ELECTRCHH ==6 | IPUMS$ELECTRCHH ==8)]<-0
IPUMS$COOKFUEL[which(IPUMS$COOKFUEL==400 | IPUMS$COOKFUEL==500 | IPUMS$COOKFUEL==520 |IPUMS$COOKFUEL==530) ] <-1
IPUMS$COOKFUEL[which(IPUMS$COOKFUEL !=1 )]<-0

#Create seperate tables for 3 time intervals
C_erase<-c("SAMPLE","URBAN","CLUSTERNO", "ELECTRCHH", "COOKFUEL")
data2003<- IPUMS[IPUMS$SAMPLE==2884, names(IPUMS) %in% C_erase]
data2008<- IPUMS[IPUMS$SAMPLE==2885, names(IPUMS) %in% C_erase]
data2014<- IPUMS[IPUMS$SAMPLE==2886, names(IPUMS) %in% C_erase]

#Aggregate to Cluster number by averaging values
d2003<-aggregate(data2003[, 4:5], list(data2003$CLUSTERNO), mean)
d2008<-aggregate(data2008[, 4:5], list(data2008$CLUSTERNO), mean)
d2014<-aggregate(data2014[, 4:5], list(data2014$CLUSTERNO), mean)

#Descriptive stats
install.packages(c("ggplot2","RColorBrewer","scales"))
library(ggplot2); library(scales); library(grid); library(RColorBrewer)

qplot(d2003$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2003")
qplot(d2008$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2008")
qplot(d2014$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2014")
qplot(d2003$COOKFUEL,
      geom="histogram",
      xlab = "Wood Use",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Wood as Cooking Fuel 2003")
qplot(d2008$COOKFUEL,
      geom="histogram",
      xlab = "Wood USe",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Wood as Cooking Fuel 2008")
qplot(d2014$COOKFUEL,
      geom="histogram",
      xlab = "Wood Use",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Wood as Cooking Fuel 2014")


#attach survey data with spatial data
pts03 <- system.file("Data/clusters2003/GHGE4BFL.shp")
ogrInfo(pts03)
roads <- readOGR(dsn = pts03)
rods<-shapefile("Data/clusters2003/GHGE4BFL.shp")
summary(rods)
?readOGR
ogrInfo(fnm)

#This is the link to download the Hansen data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/d5c909c06ec28626324ecd65c34417f2
