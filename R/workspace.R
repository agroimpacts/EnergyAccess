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
library(mapview)
library(ggplot2)
library(reshape2)
library(gridExtra) #arranging ggplots in grid
library(grid) #arranging ggplots in grid
#======================================================
### CLEANING SURVEY DATA ###
## READ/CLEAN IPUMS DATA ##
IPUMS<- read.csv(file="inst/extdata/idhs_00003.csv", stringsAsFactors = FALSE)

#Reassign into boolean values
#IPUMS<-subset(IPUMS, URBAN!=1) #activate for rural only analysis
IPUMS$ELECTRCHH[which(IPUMS$ELECTRCHH ==6 | IPUMS$ELECTRCHH ==8)]<-0
IPUMS$COOKFUEL[which(IPUMS$COOKFUEL==400 | IPUMS$COOKFUEL==500 | IPUMS$COOKFUEL==520 |IPUMS$COOKFUEL==530) ] <-1
IPUMS$COOKFUEL[which(IPUMS$COOKFUEL !=1 )]<-0
IPUMS$EDUCLVL[which(IPUMS$EDUCLVL==2 | IPUMS$EDUCLVL==3 | IPUMS$EDUCLVL==8)] <-1

#Create seperate tables for 3 time intervals
C_erase<-c("SAMPLE","URBAN","CLUSTERNO", "ELECTRCHH", "COOKFUEL", "EDUCLVL")
data2003<- IPUMS[IPUMS$SAMPLE==2884, names(IPUMS) %in% C_erase]
data2008<- IPUMS[IPUMS$SAMPLE==2885, names(IPUMS) %in% C_erase]
data2014<- IPUMS[IPUMS$SAMPLE==2886, names(IPUMS) %in% C_erase]

#Aggregate to Cluster number by averaging values
d2003<-aggregate(data2003[, 4:5], list(data2003$CLUSTERNO), mean)
d2008<-aggregate(data2008[, 4:5], list(data2008$CLUSTERNO), mean)
d2014<-aggregate(data2014[, 4:6], list(data2014$CLUSTERNO), mean)
d2003$COOKFUEL<-round(d2003$COOKFUEL, 3)*100
d2003$ELECTRCHH<-round(d2003$ELECTRCHH, 3)*100
d2008$COOKFUEL<-round(d2008$COOKFUEL, 3)*100
d2008$ELECTRCHH<-round(d2008$ELECTRCHH, 3)*100
d2014$COOKFUEL<-round(d2014$COOKFUEL, 3)*100
d2014$ELECTRCHH<-round(d2014$ELECTRCHH, 3)*100
d2014$EDUCLVL<-round(d2014$EDUCLVL, 3)*100


## READ/CLEAN DHS STATCOMPILER DATA ##
#these variables will be used for multivariate regression for 2014 only
DHS<- read.csv(file="inst/extdata/clusters2014/GHGC71FL.csv", stringsAsFactors = FALSE)
C_erase<-c("DHSCLUST","All_Population_Density_2015","BUILT_Population_2014")
dataDHS<- DHS[, names(DHS) %in% C_erase] #clean out unncessary attributes
colnames(dataDHS)<-c("DHSCLUST", "Pop15", "Built14")
dataDHS$Built14<-round(dataDHS$Built14, 4)

## MAKING SURVEY DATA SPATIAL ##
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
clust14M <- merge(clust14M, dataDHS, by.x = "DHSCLUST",
                  by.y ="DHSCLUST") #add columns for Built and Population

cnames<-c("ELECTRCHH","COOKFUEL")
cnames1<-c("ELECTRCHH","COOKFUEL","EDUCLVL","Pop15","Built14")
clust03M<- clust03M[,(names(clust03M) %in% cnames)]
clust08M<- clust08M[,(names(clust08M) %in% cnames)]
clust14M<- clust14M[,(names(clust14M) %in% cnames1)]
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
r <- raster(extent(dist_albs), res = 2000, crs = crs(dist_albs), #create blank raster
            vals = 1)

interpolCOOK <- lapply(clist, function(x) { #interpolate cluster point values to raster surface
  a <- gstat(id = "COOKFUEL", formula = COOKFUEL ~ 1, data = x)
  b <- interpolate(object = r, model = a)
  c <- mask(x = b, mask = dist_albs)
})
interpolEnergy <- lapply(clist, function(x) { #interpolate cluster point values to raster surface
  a <- gstat(id = "ELECTRCHH", formula = ELECTRCHH ~ 1, data = x)
  b <- interpolate(object = r, model = a)
  c <- mask(x = b, mask = dist_albs)
})

#EDUCATION interpolate cluster point values to raster surface
a <- gstat(id = "EDUCLVL", formula = EDUCLVL ~ 1, data = c2014)
b <- interpolate(object = r, model = a)
c <- mask(x = b, mask = dist_albs)
#POPULATION interpolate cluster point values to raster surface
a1 <- gstat(id = "Pop15", formula = Pop15 ~ 1, data = c2014)
b1 <- interpolate(object = r, model = a1)
c1 <- mask(x = b1, mask = dist_albs)

#BUILT AREAS interpolate cluster point values to raster surface
a2 <- gstat(id = "Built14", formula = Built14 ~ 1, data = c2014)
b2 <- interpolate(object = r, model = a2)
c2 <- mask(x = b2, mask = dist_albs)

## INTERPOLATED RASTER to DISTRICTS ##
dist_a<-dist_albs
#Wood as Cooking Fuel
v.vals <- extract(interpolCOOK[[1]], dist_a)
dist_a$COOKFUEL03 <- round(sapply(v.vals, mean))
v.vals <- extract(interpolCOOK[[2]], dist_a)
dist_a$COOKFUEL08 <- round(sapply(v.vals, mean))
v.vals <- extract(interpolCOOK[[3]], dist_a)
dist_a$COOKFUEL14 <- round(sapply(v.vals, mean))
#Energy Access
v.vals <- extract(interpolEnergy[[1]], dist_a)
dist_a$ELECTRCHH03 <- round(sapply(v.vals, mean))
v.vals <- extract(interpolEnergy[[2]], dist_a)
dist_a$ELECTRCHH08 <- round(sapply(v.vals, mean))
v.vals <- extract(interpolEnergy[[3]], dist_a)
dist_a$ELECTRCHH14 <- round(sapply(v.vals, mean))
#Education
v.vals <- extract(c, dist_a)
dist_a$EDUCLVL14 <- round(sapply(v.vals, mean))
#Population
v.vals <- extract(c1, dist_a)
dist_a$Pop15 <- round(sapply(v.vals, mean), 3)
#Built Areas
v.vals <- extract(c2, dist_a)
dist_a$Built14 <- round(sapply(v.vals, mean), 4)

#======================================================
#### RASTER SECTION ####
## DEFORESTATION ##
fnm5 <- file.path("C:/Users/NMcCray/Documents/R/EnergyAccess/inst/extdata/HansenAllyr.tif")
deforestation <- raster(fnm5)
zamr <- raster(x = extent(districts), crs = crs(districts), res = 0.1)
values(zamr) <- 1:ncell(zamr)

zamr_alb <- projectRaster(from = zamr, res = 2500, crs = crs(dist_a),
                          method = "ngb")

deforest_alb <- projectRaster(from = deforestation, to = zamr_alb, method = "ngb")
rclmat <- matrix( #all deforestation since 2001
  c(0, 0.9, 0, 0.99, 16, 1),
  nrow = 2,
  ncol = 3,
  byrow = TRUE)

rclmat1 <- matrix( #deforestation from 2001-2003
  c(0, 0.9, 0, 0.99, 3.9, 1, 3.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

rclmat2 <- matrix( #deforestation from 2003-2008
  c(0, 3.9, 0, 3.99, 8.9, 1, 8.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

rclmat3 <- matrix( #deforestation from 2008-2014
  c(0, 8.9, 0, 8.99, 14.9, 1, 14.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

totaldeforestclass <- reclassify(x = deforest_alb, rcl = rclmat, include.lowest = TRUE)
deforestclass0103 <- reclassify(x = deforest_alb, rcl = rclmat1, include.lowest = TRUE)
deforestclass0408 <- reclassify(x = deforest_alb, rcl = rclmat2, include.lowest = TRUE)
deforestclass0914 <- reclassify(x = deforest_alb, rcl = rclmat3, include.lowest = TRUE)

#extract values
deforest.all <-extract(totaldeforestclass, dist_a)
deforest.0103 <- extract(deforestclass0103, dist_a)
deforest.0408 <- extract(deforestclass0408, dist_a)
deforest.0914 <- extract(deforestclass0914, dist_a)

#aggregated to district
dist_a$deforestALL<-round(100*sapply(deforest.all, mean),3)
dist_a$deforest03<-round(100*sapply(deforest.0103, mean),3)
dist_a$deforest08<-round(100*sapply(deforest.0408, mean),3)
dist_a$deforest14<-round(100*sapply(deforest.0914, mean),3)


## CROPLAND ##
fnm6 <- file.path("C:/Users/NMcCray/Documents/R/EnergyAccess/inst/extdata/LandUse2009.tif")
CLand <- raster(fnm6)
CLand_alb <- projectRaster(from = CLand, to = zamr_alb, method = "ngb") #project

rcc1 <- matrix(
  c(0, 31, 1, 39, 231, 0),
  nrow = 2,
  ncol = 3,
  byrow = TRUE)

CLand_RC <- reclassify(x = CLand_alb, rcl = rcc1, include.lowest = TRUE)
CLand_RC_e <- extract(CLand_RC, dist_a)
dist_a$crop09<-round(100*sapply(CLand_RC_e, mean),3) #aggregate crop % values to district
#======================================================
### VISUALATION ###

#Descriptive stats
#scatter plots

#>>>Energy Access
elecdf = data.frame(count = c(1:139), dist_a@data[,5:7])
colnames(elecdf)<-c("count","energy access 2003","energy access 2008","energy access 2014")
elecdf.m = melt(elecdf, id.vars ="count", measure.vars = c("energy access 2003","energy access 2008","energy access 2014"))
p1<-ggplot(elecdf.m, aes(count, value, colour = variable)) + geom_point() + ylim(0,100)+stat_smooth(method=lm)+ ggtitle("Energy Access") +theme(plot.title = element_text(color="#666666", face="bold", size=23, hjust=0))+labs(x="District #",y="% of Electrified Dwellings")+theme(axis.title = element_text( color="#666666", face="bold", size=13))

#>>>Wood Use
cookdf = data.frame(count = c(1:139), dist_a@data[,2:4])
colnames(cookdf)<-c("count","wood use 2003","wood use 2008","wood use 2014")
cookdf.m = melt(cookdf, id.vars ="count", measure.vars = c("wood use 2003","wood use 2008","wood use 2014"))
p2<-ggplot(cookdf.m, aes(count, value, colour = variable)) + geom_point() + ylim(0,100)+stat_smooth(method=lm)+ ggtitle("Wood Use as Cooking Fuel") +theme(plot.title = element_text(color="#666666", face="bold", size=23, hjust=0))+labs(x="District #",y="% of Wood Use")+theme(axis.title = element_text( color="#666666", face="bold", size=13))

#>>>Deforestation
defdf = data.frame(count = c(1:139), dist_a@data[,12:14])
colnames(defdf)<-c("count","deforestation 2003","deforestation 2008","deforestation 2014")
defdf.m = melt(defdf, id.vars ="count", measure.vars = c("deforestation 2003","deforestation 2008","deforestation 2014"))
head(defdf.m)
p3<-ggplot(defdf.m, aes(count, value, colour = variable)) + geom_point() + ylim(0,15)+stat_smooth(method=lm)+ ggtitle("Deforestation") +theme(plot.title = element_text(color="#666666", face="bold", size=23, hjust=0))+labs(x="District #",y="% of Area Deforested")+theme(axis.title = element_text( color="#666666", face="bold", size=13))

grid.arrange(p1,p2,p3, nrow=3) #OUTPUT

#histograms
h1<-ggplot(data = elecdf.m, mapping = aes(x = value, fill=variable)) +
  geom_histogram(bins = 10) + facet_wrap(~variable) #energy access
h2<-ggplot(data = cookdf.m, mapping = aes(x = value, fill=variable)) +
  geom_histogram(bins = 10) + facet_wrap(~variable) #Wood
h3<-ggplot(data = defdf.m, mapping = aes(x = value, fill=variable)) +
  geom_histogram(bins = 10) + facet_wrap(~variable) #Deforestation

grid.arrange(h1,h2,h3, nrow=3) #OUTPUT

#maps
scale<-seq(0, 100, 10) #standardize legend scale
scaleD<-seq(0, 20, 2) #scale for deforesataion legends
cols<-rev(get_col_regions()) #add col.regions=cols for reveresed and new colors
Mtype<-c("CartoDB.Positron") #basemap

e_map03<-mapview(dist_a, zcol="ELECTRCHH03", col.regions=cols, layer.name="2003 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype)
e_map08<-mapview(dist_a, zcol="ELECTRCHH08", col.regions=cols, layer.name="2008 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype)
e_map14<-mapview(dist_a, zcol="ELECTRCHH14", col.regions=cols, layer.name="2014 Energy Access", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype)
ElecMaps=e_map03+ e_map08 +e_map14
ElecMaps #OUTPUT

c_map03<-mapview(dist_a, zcol="COOKFUEL03", layer.name="2003 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype, col.regions=cols)
c_map08<-mapview(dist_a, zcol="COOKFUEL08", layer.name="2008 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype, col.regions=cols)
c_map14<-mapview(dist_a, zcol="COOKFUEL14", layer.name="2014 Wood Use", maxpoints=40000000, alpha.regions=100,legend=TRUE, at= scale, map.types=Mtype, col.regions=cols)
CookMaps<-c_map03+c_map08+c_map14
CookMaps #OUTPUT

d_map03<-mapview(dist_a, zcol="deforest03", layer.name="2003 Deforestation", maxpoints=40000000, alpha.regions=100,legend=TRUE,col.regions=cols, at= scaleD, map.types=Mtype)
d_map08<-mapview(dist_a, zcol="deforest08", layer.name="2008 Deforestation", maxpoints=40000000, alpha.regions=100,legend=TRUE,col.regions=cols, at= scaleD, map.types=Mtype)
d_map14<-mapview(dist_a, zcol="deforest14", layer.name="2014 Deforestation", maxpoints=40000000, alpha.regions=100,legend=TRUE,col.regions=cols, at= scaleD, map.types=Mtype)
defMaps<-d_map03+d_map08+d_map14
defMaps  #OUTPUT

#======================================================
### ANALYIS ###
##Multivariate Regression##
head(dist_a)
fit<-lm(deforestALL ~ COOKFUEL14 + ELECTRCHH14 + Pop15 + EDUCLVL14 + Built14 + crop09, data= dist_a)
summary(fit)
coefficients(fit)
confint(fit, level=0.95) #confidence intervals
fitted(fit)
plot(residuals(fit))
anova(fit)
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

library(MASS)
step<- stepAIC(fit, direction="both")
step$anova
library(foreign)
dist_aCSV<-as.data.frame(dist_a)
head(dist_a)



##OLS###
### TIME SERIES LINERA REGRESSION##### tslm(formula=)
#splm (for spatial regressions)##
#rvest- harvest scrape webpages#
####BIVARIATE LOCAL SPATIAL AUTOCORRELATION####
#bivariate Morans
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(sf)
install.packages("spdep")
library(spdep)
library(rgdal)
library(stringr)
y<- dist_a$ELECTRCHH03
x<- dist_a$deforest03
head(dist_a@data)
# Programming some functions

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x

  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
  W[which(is.na(W))] <- 0
  n <- nrow(W)

  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 1000){

  if(is.null(y)) y = x

  n   = nrow(W)
  IDs = 1:n

  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  W[which(is.na(W))] <- 0

  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)

  ID_sample = sample(IDs, size = n*nsims, replace = T)
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)

  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)

  list(global_sims = global_sims,
       local_sims  = local_sims)
}


#======================================================
# Adjacency Matrix (Queen)

nb <- poly2nb(dist_a)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0

#======================================================
# Calculating the index and its simulated distribution
# for global and local values

m   <- moran_I(x, y, W)
m[[1]] # global value

m_i <- m[[2]]  # local values

local_sims <- simula_moran(x, y, W)$local_sims

# Identifying the significant values
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

#======================================================
# Preparing for plotting

dist_a03<- st_as_sf(dist_a)
dist_a03$sig <- sig


# Identifying the LISA patterns
xp <- (x-mean(x))/sd(x)
yp <- (y-mean(y))/sd(y)

patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>%
  str_replace_all("TRUE","High") %>%
  str_replace_all("FALSE","Low")
patterns[dist_a03$sig==0] <- "Not significant"
dist_a03$patterns <- patterns

# Plotting
mapview(dist_a03, zcol="patterns", legend=TRUE, alpha=0, maxpoints=40000000, alpha.regions=80, layer.name="BiLISA: Deforestation and EA")

#This is the link to download the Hansen data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/d5c909c06ec28626324ecd65c34417f2

#This is the link to download the Cropland data
#Go to tasks and then download to google drive
#https://code.earthengine.google.com/594731702af6ef064128e784a632a0e8
