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

fnm <- system.file("extdata/HansenAllyr.tif", package = "EnergyAccess")
deforestation <- raster(fnm)

#fnm <- system.file("extdata/DistrictBoundary/GHA_admbndp2_1m_GAUL.shp", package = "EnergyAccess")
#fnm <- system.file("extdata/GHA_admbndp2_1m_GAUL.shp", package = "EnergyAccess")

districts <- readOGR(dsn = fnm, layer = GHA_admbndp2_1m_GAUL)
