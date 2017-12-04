#Description: Recent research has identified a relationship between deforestation rates and
#energy access in rural areas of developing countries. Basic energy sources typically are disperse
#and require manual labor and time to collect and process, wood being the most common in many rural
#areas without energy access. When these energy sources are exploited more quickly than they can
#regrow, deforestation pressures can become detrimental to the environment and to the sustainment
#of the traditional lifestyle. Adoption of more advanced forms of energy fuel, such as electricity,
#improve livelihoods and decrease deforestation rates.

#This research project uses Ghana as a case study to test the correlation between rural
#electrification and deforestation because of the rapid 54.4% increase in energy access the nation
#has experienced from 1990-2014. The project will separately assess rates of wood use through
#reporting on cooking fuel type, rates of electrification and rates of deforestation by district.
#The relationship between energy access and deforestation will then be analyzed at the district-level.
#Our research will improve upon previous research which was conducted at the national-scale.
#Household survey data and Hansen deforestation data will be used to explore the relationships of
#interest.

library(devtools)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(gstat)
library(roxygen2)
library(ggplot2)

#install_github(repo = "agroimpacts/EnergyAccess", ref = "dml",
#auth_token = "0ded53980c6a7503284cab8e422efc67d0cb5e80")

#Read in deforestation raster and district boundary vector

fnm <- system.file("extdata/HansenAllyr.tif", package = "EnergyAccess")
deforestation <- raster(fnm)

fnm2 <- system.file("extdata/DistrictBoundary/GHA_admbndp2_1m_GAUL.shp", package = "EnergyAccess")
districts <- readOGR(dsn = fnm2, layer = "GHA_admbndp2_1m_GAUL")

#Should aggregate to districts first, don't need to rasterize
#Rasterize district boundary vector
#zamr <- raster(x = extent(districts), crs = crs(districts), res = 0.1)
#values(zamr) <- 1:ncell(zamr)
#districts$ID <- 1:length(districts)
#districtsraster <- rasterize(x = districts, y = zamr, field = "ID")
#districtsraster

#plot_noaxes

plot_noaxes <- function(x, axes = FALSE, box = FALSE, mar = c(0, 0, 1, 4),
                        ...) {
  if(!class(x) %in% c("RasterLayer", "RasterStack", "RasterBrick", "Extent")) {
    stop("This function is intended for rasters only", call. = FALSE)
  }
  par(mar = mar)
  plot(x, axes = axes, box = axes, ...)
}

#get them to all the same coordinate system (this didn't quite work)

dist_albs <- spTransform(x=districts, CRS="+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0
                         +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

zamr_alb <- projectRaster(from = zamr, res = 2500, crs = crs(dist_albs),
                          method = "ngb")

deforest_alb <- projectRaster(from = deforestation, to = zamr_alb, method = "ngb")




#Reclassify all deforestation to the yearly categories in deforestation raster

rclmat <- matrix(
  c(0, 0.9, 0, 0.99, 16, 1),
  nrow = 2,
  ncol = 3,
  byrow = TRUE)

rclmat1 <- matrix(
  c(0, 0.9, 0, 0.99, 3.9, 1, 3.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

rclmat2 <- matrix(
  c(0, 3.9, 0, 3.99, 8.9, 1, 8.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

rclmat3 <- matrix(
  c(0, 8.9, 0, 8.99, 14.9, 1, 14.99, 16, 0),
  nrow = 3,
  ncol = 3,
  byrow = TRUE)

totaldeforestclass <- reclassify(x = deforest_alb, rcl = rclmat, include.lowest = TRUE)

deforestclass0103 <- reclassify(x = deforest_alb, rcl = rclmat1, include.lowest = TRUE)

deforestclass0408 <- reclassify(x = deforest_alb, rcl = rclmat2, include.lowest = TRUE)

deforestclass0914 <- reclassify(x = deforest_alb, rcl = rclmat3, include.lowest = TRUE)

#Summarize deforestation statistics by district
#Need to find resolution and coordinate system of raster
#Possibly do some resampling

deforest.total <- extract(totaldeforestclass, dist_albs)
deforest.0103 <- extract(deforestclass0103, dist_albs)
deforest.0408 <- extract(deforestclass0408, dist_albs)
deforest.0914 <- extract(deforestclass0914, dist_albs)

districts$deforest.total <- 100*sapply(deforest.total, mean)
districts$deforest.0103 <- 100*sapply(deforest.0103, mean)
districts$deforest.0408 <- 100*sapply(deforest.0408, mean)
districts$deforest.0914 <- 100*sapply(deforest.0914, mean)

#districts$deforest.total <- 100*districts$deforest.total
#districts$deforest.0103 <- 100*districts$deforest.0103
#districts$deforest.0408 <- 100*districts$deforest.0408
#districts$deforest.0914 <- 100*districts$deforest.0914

#spplot(districts, z = "deforest.total")
#spplot(districts, z = "deforest.0103")
#spplot(districts, z = "deforest.0408")
#spplot(districts, z = "deforest.0914")
