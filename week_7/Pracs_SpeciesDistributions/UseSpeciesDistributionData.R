#######################################
## this is for practice how to use ICUN range maps and GBIF occurrences
# download IUCN range maps; read range maps; map range maps; rasterize range maps etc.
# and download GBIF occurrences and generate range maps;
# contact: Wubing Xu (wubing.xu@idiv.de) -- 2023.06.11

rm(list = ls())

# set your working directory
setwd("C:/Dropbox/iDiv/Courses/BScBioinformatics_2023/Pracs_SpeciesDistributions")

#install and load packages
packages <- c("raster","maps","sp", "rgeos","letsR", "rgbif", "CoordinateCleaner", "maptools", "alphahull", "dplyr")
package.check <- lapply(packages, FUN=function(x)
{
  if(!require(x, character.only=TRUE)){
    install.packages(x, dependencies=TRUE)
    library(x, character.only=TRUE)
  }
}
)

####################
## 1. Use IUCN range maps

# you can download expert range maps of mammals, birds, amphibians, reptiles, and freshwater fishes and some plants and marine species
# from the IUCN website (https://www.iucnredlist.org/resources/spatial-data-download)
# they are fully open accessible, only require registration

# Read shapefile for the maple  species
maple <- shapefile('data/MAPLES/MAPLES.shp')

# this is a SpatialPolygonsDataFrame with many polygons (ranges). The attribute table contains information for each polygon
maple
head(maple@data)
unique(maple@data$sci_name) # names of species (16 species)

table(maple@data$sci_name)
subset(maple@data, sci_name=='Acer opalus')


## we use a species as an example
acer_mon <- maple[maple@data$sci_name=='Acer monspessulanum',]
acer_mon

# Map the range 
# the world map using the maps package
map('world', fill = TRUE, col = "gray")

# Overlay the range of Acer monspessulanum
plot(acer_mon, col='red', add=T)

# zoom to the Europe
map('world', xlim=c(-10,30), ylim=c(35,60), fill = TRUE, col = "gray")
map.axes()
plot(acer_mon, col='red', add=T)


## map ranges of all species
map('world', fill = TRUE, col = "gray")
nsp = length(unique(maple@data$sci_name)) # the number of species
cols <- rainbow(n = nsp) # set the colors
plot(maple, col= cols[as.factor(maple@data$sci_name)], add=T)

# zoom to the North America
map('world', xlim=c(-180,-50), ylim=c(20, 90), fill = TRUE, col = "gray")
map.axes()
cols <- rainbow(n = nsp, alpha = 0.5)
plot(maple, col= cols[as.factor(maple@data$sci_name)], add=T)


####################
## 2 Working with range maps
## 2.1 Range size

# we can calculate area of polygon (range) using the function area from raster package
# the area returned is in square meters (if the CRS is in long/lat format)
raster::area(acer_mon)

# Range area in square kilometers:
raster::area(acer_mon)/1000000

# Range area of multiple species
maple_area <- raster::area(maple)/1000000
maple_areas <- data.frame(maple@data[ , c("sci_name", "category")], area = maple_area)
maple_areas

# range map of the vulnerable species
acer_div <- maple[maple@data$sci_name=='Acer divergens',]
map('world', fill = TRUE, col = "gray")
plot(acer_div, col='red', add=T)

# the range is too small to see in the worldwide map, so map it regionally
plot(acer_div, col='red', axes = TRUE)

map('world', xlim=c(-10,50), ylim=c(35,60), fill = TRUE, col = "gray")
map.axes()
plot(acer_div, col='red', add=T)



## 2.2 crop range polygons
# Let's crop the range polygons of Acer monspessulanum to France

# get the map of France
data(wrld_simpl)
wrld_simpl
france <- wrld_simpl[wrld_simpl@data$NAME == "France", ]
plot(france)

# crop range polygons
acer_mon_france <- rgeos::gIntersection(acer_mon, france)

# Map the cropped range
map(database = france, fill = TRUE, col = "gray")
map.axes()
plot(acer_mon_france,col='red',add=T)



## 2.3 Rasterizing range maps
# For many applications in macroecology, we need raster of polygons in specific resolutions.

## 2.3.1 Rasterizing range maps using package raster
# By default, raster() will create a 1 degree resolution map in the *WGS 84* coordinate system (lon/lat).
r_1deg <- raster(res = 1)
r_1deg

# transfer the polygon data to the raster cells
acer_mon_1deg <- rasterize(acer_mon, r_1deg)
acer_mon_1deg

map('world', xlim=c(-10,30), ylim=c(35,60), fill = TRUE, col = "gray")
map.axes()
plot(acer_mon, col='red', add=T) # the range polygon
plot(acer_mon_1deg, add=T, alpha=0.8, legend=F) # the raster
# note: some areas of polygon are not transfer to raster cells as they don't covers the center of a raster cell


# 2.3.2 Rasterising range maps with package letsR
# The lets.presab() function in package letsR need a column with name as "binomial"
acer_mon@data$binomial <- acer_mon@data$sci_name

# We set the resolution to 1 degree (the default) and restrict the spatial extent to Europe
r_acer_mon <- letsR::lets.presab(acer_mon, resol=1, xmn = -10, xmx = 30, ymn = 35, ymx = 60)

# Map the range
map('world', xlim=c(-10,30), ylim=c(35,60), fill = TRUE, col = "gray")
map.axes()
plot(acer_mon, col='red', add=T)
plot(r_acer_mon, add=T, world = FALSE, alpha=0.8, legend=F)



## 2.3.3 Bulk rasterize multiple species' range maps using letsR

# add the column "binomial"
maple@data$binomial <- maple@data$sci_name

# rasterise range maps
r_maple <- lets.presab(maple, resol=1)

# Look at structure of the object 
str(r_maple, 1)

head(r_maple[[1]])
r_maple[[2]]
r_maple[[3]]

# Plot species richness
plot(r_maple)


# Map distribution of individual species
par(mfrow=c(1, 2)) # 6 8
plot(r_maple, name = "Acer monspessulanum")
plot(r_maple, name = "Acer saccharinum")

plot(r_maple, name = "Acer monspessulanum", xlim = c(-10, 30), ylim = c(35, 60))
plot(r_maple, name = "Acer saccharinum", xlim=c(-180,-50), ylim=c(20, 90))
par(mfrow=c(1, 1))




####################
## 3 Use occurrences from GBIF

## 3.1 get GBIF occurrences and map them 
# We will find the occurrences of the species Acer monspessulanum

# Use name_suggest to find the species ID in GBIF. Just make sure the name can be identified by GBIF
# You can also use the species names directly
acer_mon_gbif_name <- name_suggest(q='Acer monspessulanum', rank='species')$data
acer_mon_gbif_name
acer_mon_gbif_key <- acer_mon_gbif_name$key[1]

# Search GBIF occurrences. There are many parameters to filter data
# Here we only restrict occurrences with coordinates, having no geographic issues and distributed in Erope.
acer_mon_gbif_occ <- occ_search(taxonKey=acer_mon_gbif_key, hasCoordinate=TRUE, hasGeospatialIssue = FALSE, continent = 'Europe', limit=1000)$data
acer_mon_gbif_occ

## note: for a formal research project, occ_download() should be used, but it require a username or password
#  occ_search() can only download a maximum of 100,000 records


# clean gbif occurrences based on column names
acer_mon_gbif_occ <- acer_mon_gbif_occ %>% 
  setNames(tolower(names(.))) %>% # change column names to lower cases
  filter(occurrencestatus  == "PRESENT") %>%
  filter(!basisofrecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")) %>%
  filter(!basisofrecord == "MATERIAL_SAMPLE") %>%  #remove *most* metagenomics records 
  filter(!taxonrank == "UNRANKED") %>% 
  filter(coordinateuncertaintyinmeters <= 100000 | is.na(coordinateuncertaintyinmeters))

# clean gbif occurrences using functions from package CoordinateCleaner
acer_mon_gbif_occ <- acer_mon_gbif_occ %>%
  cc_gbif(buffer = 2000) %>% 
  clean_coordinates(tests = c("capitals","centroids","equal","institutions","zeros"), 
                    capitals_rad = 2000, centroids_detail = "country", value = "clean")

# look some interested columns
acer_mon_gbif_occ[, c("scientificname", "decimallatitude", "decimallongitude", "basisofrecord", "year", "country" )]
acer_mon_gbif_occ2 <- acer_mon_gbif_occ[, c("scientificname", "decimallatitude", "decimallongitude", "basisofrecord", "year", "country" )]

# transfer the table into SpatialPointsDataFrame
coordinates(acer_mon_gbif_occ2) <- ~decimallongitude + decimallatitude
projection(acer_mon_gbif_occ2) <- "+proj=longlat +datum=WGS84 +no_defs "
acer_mon_gbif_occ2

# Map distribution occurrences
map('world', xlim=c(-10, 30), ylim=c(35, 60), fill = TRUE, col = "gray")
plot(acer_mon, add =TRUE, col = "red") # the IUCN expert range map
plot(acer_mon_gbif_occ2, col = "GREEN", add =TRUE) # GBIF point observations


## transfer occurrences into raster cells
# create a 1 degree resolution raster within the extent of Europe 
r_eu <- raster(xmn =-10, xmx =30, ymn =35, ymx =60, res = 1)

# transfer the points data to the raster cells: calculate the number of occurrences in each cell
r_acer_mon_gbif <- rasterize(x = acer_mon_gbif_occ2, y = r_eu, field = "year", fun='count')
r_acer_mon_gbif

# map the raster of number of occurrences
map('world', xlim=c(-10, 30), ylim=c(35, 60), fill = TRUE, col = "gray")
plot(r_acer_mon_gbif, add =TRUE, col = topo.colors(n = 5, rev =TRUE))



## 3.1  generate range map based on occurrence points

# input my defined functions, which will be used to construct alpha hulls
source("self_defined_functions/99_ah2sp.R")
source("self_defined_functions/99_ah_range.R")

# the function require the x (longitude) and y (latitude)
acer_mon_gbif_xy <- unique(acer_mon_gbif_occ[, c("decimallongitude", "decimallatitude")])
colnames(acer_mon_gbif_xy) <- c("x", "y")

# generate alpha hulls.The parameter alpha will affect the constructed alpha hulls: a higher value of alpha, a larger hull
data(wrld_simpl)
acer_mon_gbif_ahull <- ah_range(xy=acer_mon_gbif_xy, alpha=5, buff=20, exclude_map = wrld_simpl, is.write=FALSE)[[1]] # the warnings doesn't matter
acer_mon_gbif_ahull

# maps the constructed range map
map('world', xlim=c(-10, 30), ylim=c(35, 60), fill = TRUE, col = "gray")
plot(acer_mon_gbif_ahull, add =TRUE, col = "blue")
# add occurrence points
plot(acer_mon_gbif_occ2, col = "red", add =TRUE)


# compare the distributions from GBIF and expert range maps from IUCH
map('world', xlim=c(-10, 30), ylim=c(35, 60), fill = TRUE, col = "gray")
plot(acer_mon_gbif_ahull, add =TRUE, col = "blue") # alpha hull
plot(acer_mon, add =TRUE, col = "red") # expert range map
plot(acer_mon_gbif_occ2, col = "green", add =TRUE) # point observation



####################
## Exercise

# option 1: 
# practice using range maps of Birches; or download range maps of a small group of species from IUCN (https://www.iucnredlist.org/resources/spatial-data-download)
# to download IUCN range maps, you need to register firstly
# read the data into R, check the data structure, map distributions of one or more species, calculate range areas, etc.

# option 2: 
# download GBIF occurrences for a species that you are interested
# clean and map these occurrences; then rasterize occurrences; generate a range map as alpha hull

