library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(devtools)
library(roxygen2)
library(gstat)
library(dismo)
library(RColorBrewer)
library(viridis)

### CLEANING SURVEY DATA ###
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

#Descriptive stats
library(ggplot2); library(grid)

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


### MAKING SURVEY DATA SPATIAL ###
#Import survey cluster points
clust03<-shapefile("inst/extdata/clusters2003/c2003c.shp") #2003 Survey
clust08<-shapefile("inst/extdata/clusters2008/c2008c.shp") #2008 Survey
clust14<-shapefile("inst/extdata/clusters2014/c2014c.shp") #2014 Survey

#attach survey data averages to cluster points
clust03M <- merge(clust03, d2003, by.x = "DHSCLUST",
                  by.y = "Group.1")
clust08M <- merge(clust08, d2008, by.x = "DHSCLUST",
                 by.y = "Group.1")
clust14M <- merge(clust14, d2014, by.x = "DHSCLUST",
                 by.y = "Group.1")

#Import shapefile of Ghana districts
districts<-shapefile("inst/extdata/DistrictBoundry/GHA_admbndp2_1m_GAUL.shp")

#toying with creating interactive maps
install.packages("rworldmap")
library("rworldmap")
install.packages("mapview")
library("mapview")
install.packages("ggmap")
library(ggmap)
base_map <- get_map(location = "Ghana", zoom = 7, color = "color") #or bw
ggmap(base_map)
mapview(vca)

#project files to Albers Equal Area
dist_albs<-spTransform(x=districts, CRS="+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0
                       +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
c2003<-spTransform(x=clust03M, CRSobj=proj4string(dist_albs))
c2008<-spTransform(x=clust08M, CRSobj=proj4string(dist_albs))
c2014<-spTransform(x=clust14M, CRSobj=proj4string(dist_albs))

#Inverse Distance Weighing Interpolation of points
r <- raster(extent(dist_albs), res = 5000, crs = crs(dist_albs),
            vals = 1)
invdist <- gstat(id = "COOKFUEL", formula = COOKFUEL ~ 1, data = c2003)
invdistr <- interpolate(object = r, model = invdist)
invdistrmsk <- mask(x = invdistr, mask = dist_albs)
plot(invdistrmsk)
#kriging (not working for COOKFUEL 2003)
v6 <- variogram(object = COOKFUEL ~ 1, data = c2003)
m <- fit.variogram(object = v6, model = vgm("Sph"))
ordkrig <- gstat(id = "COOKFUEL", formula = COOKFUEL ~ 1, data = c2003, model= m)
ordkrigr <- interpolate(object = r, model = ordkrig)
ordkrigrmsk <- mask(x = ordkrigr, mask = dist_albs)
plot(ordkrigrmsk)

#visualization of results with voronoi polygons
allyrs<-c(c2003, c2008, c2014)
ghana <- aggregate(dist_albs)
v_allyrs<-lapply (allyrs, function(x) {
  v<- voronoi(x)
  intersect(v, ghana)
})
v_allyrs
spplot(v_allyrs)
v <- voronoi(c2003)
vca <- intersect(v, ghana)
ggplot(v_allyrs[[1]]) +
  geom_polygon(aes(fill = rate), colour = alpha("white", 1 / 2), size = 0.2) +
  geom_polygon(data = v_allyrs, colour = "white", fill = NA) +
  scale_fill_viridis(option="magma")

?ggplot

p03<-spplot(v_allyrs[[1]], 'COOKFUEL', col.regions=rev(get_col_regions()))
p08<-spplot(v_allyrs[[2]], 'COOKFUEL', col.regions=rev(get_col_regions()))
p14<-spplot(v_allyrs[[3]], 'COOKFUEL', col.regions=rev(get_col_regions()))
?spplot

# Make a composite plot:
# see ?print.trellis for more details
print(p03, position = c(0,.5,.5,1),more=T)
print(p08, position = c(.5,.5,1,1),more = T)
print(p14, position = c(0,0,1,.5))
#extract values from voronoi to districts so there arent too many 1's and 0's
experiment<-over(x=dist_albs, y=vca[21:22], fn=mean)
class(experiment)
newdist<- dist_albs
newdist$COOKFUEL<-experiment[,"COOKFUEL"]
newdist$ELECTRCHH<-experiment[,"ELECTRCHH"]
spplot(newdist, z="COOKFUEL", col.regions=rev(get_col_regions()), col="transparent")

#This is the link to download the Hansen data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/d5c909c06ec28626324ecd65c34417f2
