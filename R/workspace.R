library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(devtools)
library(roxygen2)
library(gstat)


#Read in IPUMS Data

IPUMS<- read.csv(file="inst/extdata/idhs_00003.csv", stringsAsFactors = FALSE)

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

########TESTING###################################
# summary of expense and csat columns, all rows
dd3<- subset(d2003, select = c("ELECTRCHH", "COOKFUEL"))
summary(dd3)
# correlation between expense and csat
cor(dd3)
plot(dd3)
dd14<- subset(d2014, select = c("ELECTRCHH", "COOKFUEL"))
summary(dd14)
# correlation between expense and csat
cor(dd8)
plot(dd14)
################################################

#Descriptive stats
library(ggplot2); library(grid); library(RColorBrewer)

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


#Attach survey data with spatial data

clust03<-shapefile("inst/extdata/clusters2003/c2003c.shp")
clust08<-shapefile("inst/extdata/clusters2008/c2008c.shp")
clust14<-shapefile("inst/extdata/clusters2014/c2014c.shp")
fnm <- system.file("inst/extdata/clusters2014/c2014c.shp")
c2003 <- readOGR(dsn = fnm, layer = "2003")

#attach survey data averages to cluster points
clust03M <- merge(clust03, d2003, by.x = "DHSCLUST",
                  by.y = "Group.1")
clust08M <- merge(clust08, d2008, by.x = "DHSCLUST",
                 by.y = "Group.1")
clust14M <- merge(clust14, d2014, by.x = "DHSCLUST",
                 by.y = "Group.1")
clusterlist<-list(clust03M, clust08M, clust14M)

districts<-shapefile("inst/extdata/DistrictBoundry/GHA_admbndp2_1m_GAUL.shp")

#project files to Albers Equal Area
dist_albs<-spTransform(x=districts, CRS="+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0
                       +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
lapply (clusterlist, function(x)
  spTransform(x=x, CRSobj=proj4string(dist_albs)))

r <- raster(extent(districts), res = 5000, crs = crs(dist_albs),
            vals = 1)
indist<- lapply(clusterlist, function(x)
  gstat(id=clusterlist[x],))

invdist <- gstat(id = "2003", formula = c03 ~ 1, data = c03)
invdistr <- interpolate(object = r, model = invdist)
invdistrmsk <- mask(x = invdistr, mask = raintotalb)



?gstat
#ogrInfo(pts03)
#roads <- readOGR(dsn = pts03)
#rods<-shapefile("Data/clusters2003/GHGE4BFL.shp")
#summary(rods)
#?readOGR
#ogrInfo(fnm)

#This is the link to download the Hansen data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/d5c909c06ec28626324ecd65c34417f2
