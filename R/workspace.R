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
head(IPUMS)
qplot(IPUMS$URBAN,
      geom="histogram",
      xlab = "Job",
      binwidth= 0.2,
      fill=I("blue"),
      col=I("red"),
      main = "employment")
#Reassign into boolean values
#IPUMS<-subset(IPUMS, URBAN!=1) #activate for rural only analysis
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
d2003$COOKFUEL<-round(d2003$COOKFUEL, 3)*100
d2003$ELECTRCHH<-round(d2003$ELECTRCHH, 3)*100
d2008$COOKFUEL<-round(d2008$COOKFUEL, 3)*100
d2008$ELECTRCHH<-round(d2008$ELECTRCHH, 3)*100
d2014$COOKFUEL<-round(d2014$COOKFUEL, 3)*100
d2014$ELECTRCHH<-round(d2014$ELECTRCHH, 3)*100

#Descriptive stats
library(ggplot2); library(grid)

qplot(d2003$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 15,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2003")
qplot(d2008$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 15,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2008")
qplot(d2014$ELECTRCHH,
      geom="histogram",
      xlab = "Access",
      binwidth= 15,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Energy Access 2014")
qplot(d2003$COOKFUEL,
      geom="histogram",
      xlab = "Wood Use",
      binwidth= 15,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Wood as Cooking Fuel 2003")
qplot(d2008$COOKFUEL,
      geom="histogram",
      xlab = "Wood Use",
      binwidth= 15,
      fill=I("blue"),
      col=I("red"),
      main = "Ghana Wood as Cooking Fuel 2008")
qplot(d2014$COOKFUEL,
      geom="histogram",
      xlab = "Wood Use",
      binwidth= 15,
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
cnames<-c("ELECTRCHH","COOKFUEL")
clust03M<- clust03M[,(names(clust03M) %in% cnames)]
clust08M<- clust08M[,(names(clust08M) %in% cnames)]
clust14M<- clust14M[,(names(clust14M) %in% cnames)]
clust03M <- clust03M[!is.na(clust03M@data$COOKFUEL),]
clust08M <- clust08M[!is.na(clust08M@data$COOKFUEL),]
clust14M <- clust14M[!is.na(clust14M@data$COOKFUEL),]

#Import shapefile of Ghana districts
districts<-shapefile("inst/extdata/DistrictBoundary/GHA_admbndp2_1m_GAUL.shp")
districts <- districts[,(names(districts) %in% "HRname")] #keep column with district names

#project files to Albers Equal Area
dist_albs<-spTransform(x=districts, CRS="+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0
                       +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
c2003<-spTransform(x=clust03M, CRSobj=proj4string(dist_albs))
c2008<-spTransform(x=clust08M, CRSobj=proj4string(dist_albs))
c2014<-spTransform(x=clust14M, CRSobj=proj4string(dist_albs))
clist<-c(c2003, c2008, c2014)

#Inverse Distance Weighing Interpolation of points
r <- raster(extent(dist_albs), res = 2000, crs = crs(dist_albs),
            vals = 1)

interpolCOOK <- lapply(clist, function(x) {
  a <- gstat(id = "COOKFUEL", formula = COOKFUEL ~ 1, data = x)
  b <- interpolate(object = r, model = a)
  c <- mask(x = b, mask = dist_albs)
})
interpolEnergy <- lapply(clist, function(x) {
  a <- gstat(id = "ELECTRCHH", formula = ELECTRCHH ~ 1, data = x)
  b <- interpolate(object = r, model = a)
  c <- mask(x = b, mask = dist_albs)
})

dist_a<-dist_albs
v.vals <- extract(interpolCOOK[[3]], dist_a)
dist_a$COOKFUEL14 <- sapply(v.vals, mean)
v.vals <- extract(interpolCOOK[[2]], dist_a)
dist_a$COOKFUEL08 <- sapply(v.vals, mean)
v.vals <- extract(interpolCOOK[[1]], dist_a)
dist_a$COOKFUEL03 <- sapply(v.vals, mean)

v.vals <- extract(interpolEnergy[[3]], dist_a)
dist_a$ELECTRCHH14 <- sapply(v.vals, mean)
v.vals <- extract(interpolEnergy[[2]], dist_a)
dist_a$ELECTRCHH08 <- sapply(v.vals, mean)
v.vals <- extract(interpolEnergy[[1]], dist_a)
dist_a$ELECTRCHH03 <- sapply(v.vals, mean)

statsss<-lm(COOKFUEL03 ~ deforest03, data=dist_albs)
summary(statsss)
### VISUALIZATION ###
#data displayed in voronoi polygons
allyrs<-c(c2003, c2008, c2014)
ghana <- aggregate(dist_albs)
v_allyrs<-lapply (allyrs, function(x) {
  v<- voronoi(x)
  intersect(v, ghana)
})
#interactive maps of voronoi polygons
library(mapview)
cols<-rev(get_col_regions())
vc_map03<-mapview(v_allyrs[[1]], zcol="COOKFUEL", popup=NA, layer.name="2003 Wood Use", alpha.regions=100, col="gray27", legend=TRUE, col.regions=cols)
vc_map08<-mapview(v_allyrs[[2]], zcol="COOKFUEL", popup=NA, layer.name="2008 Wood Use", alpha.regions=100, col="gray27", legend=TRUE, col.regions=cols)
vc_map14<-mapview(v_allyrs[[3]], zcol="COOKFUEL", popup=NA, layer.name="2014 Wood Use", alpha.regions=100, col= "gray27", legend=TRUE, col.regions=cols)
CookvMaps=vc_map03+vc_map08+vc_map14
CookvMaps
ve_map03<-mapview(v_allyrs[[1]], zcol="ELECTRCHH", popup=NA, layer.name="2003 Energy Access", alpha.regions=100, col= "gray27", legend=TRUE, col.regions=cols)
ve_map08<-mapview(v_allyrs[[2]], zcol="ELECTRCHH", popup=NA, layer.name="2008 Energy Access", alpha.regions=100, col= "gray27", legend=TRUE, col.regions=cols)
ve_map14<-mapview(v_allyrs[[3]], zcol="ELECTRCHH", popup=NA, layer.name="2014 Energy Access", alpha.regions=100, col= "gray27", legend=TRUE, col.regions=cols)
ElecvMaps=ve_map03+ve_map08+ve_map14
ElecvMaps
webshot::install_phantomjs()
mapshot(CookvMaps, file = "my_interactive_map.html") #create html
?mapview

#extract values from voronoi to districts so there arent too many 1's and 0's
v_district<-lapply(v_allyrs, function(x) {
  over(x=dist_albs, y=x[1:2], fn=mean)
})
v_allyrs[1]
d03<-as.data.frame(v_district[1])
d08<-as.data.frame(v_district[2])
d14<-as.data.frame(v_district[3])
dist_albs$COOKFUEL03<-round(d03[,"COOKFUEL"],3)
dist_albs$ELECTRCHH03<-round(d03[,"ELECTRCHH"],3)
dist_albs$COOKFUEL08<-round(d08[,"COOKFUEL"],3)
dist_albs$ELECTRCHH08<-round(d08[,"ELECTRCHH"],3)
dist_albs$COOKFUEL14<-round(d14[,"COOKFUEL"],3)
dist_albs$ELECTRCHH14<-round(d14[,"ELECTRCHH"],3)

#interactive maps of districts
e_map03<-mapview(dist_albs, zcol="ELECTRCHH03", layer.name="2003 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE)
e_map08<-mapview(dist_albs, zcol="ELECTRCHH08", layer.name="2008 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE)
e_map14<-mapview(dist_albs, zcol="ELECTRCHH14", layer.name="2014 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE)
ElecMaps=e_map03+ e_map08 +e_map14
ElecMaps
c_map03<-mapview(dist_albs, zcol="COOKFUEL03", layer.name="2003 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE)
c_map08<-mapview(dist_albs, zcol="COOKFUEL08", layer.name="2008 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE)
c_map14<-mapview(dist_albs, zcol="COOKFUEL14", layer.name="2014 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE)
CookMaps<-c_map03+c_map08+c_map14
CookMaps

### ANALYIS ###
summary(dist_albs)
sts.ex.sat <- subset(dist_albs@data, select = c("COOKFUEL08", "ELECTRCHH08"))
summary(sts.ex.sat)
cor(sts.ex.sat)
dist_albs@data
# correlation between expense and csat
plot(sts.ex.sat)
sat.mod <- lm(COOKFUEL ~ ELECTRCHH, # regression formula
              data=d14) # data set
# Summarize and print the results
summary(sat.mod)
plot(sat.mod)
#This is the link to download the Hansen data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/d5c909c06ec28626324ecd65c34417f2
