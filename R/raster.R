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

#Rasterize district boundary vector
zamr <- raster(x = extent(districts), crs = crs(districts), res = 0.1)
values(zamr) <- 1:ncell(zamr)
districts$ID <- 1:length(districts)
districtsraster <- rasterize(x = districts, y = zamr, field = "ID")
districtsraster

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
zamr_alb <- projectRaster(from = zamr, res = 5000, crs = crs(districts),
                       method = "ngb")
deforest_alb <- projectRaster(from = deforestation, to = zamr_alb, res = 5000, crs = crs(districts),
                              method = "ngb")

#Reclassify all deforestation to 1 category in deforestation raster

rclmat <- matrix(
  c(0, 1, 0, 1, 15, 1),
  nrow = 2,
  ncol = 3,
  byrow = TRUE)

deforestclass <- reclassify(x = deforestation, rcl = rclmat, include.lowest = TRUE)

#Summarize deforestation statistics by district
#Need to find resolution and coordinate system of raster
#Possibly do some resampling
