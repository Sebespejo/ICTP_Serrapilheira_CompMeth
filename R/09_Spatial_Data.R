library("sf")
library(tmap)
library(dplyr)
library(raster)

data(World)
# package tmap has a syntax similar to ggplot.
#The functions start all with tm_
tm_shape(World,projection="+proj=robin") +
  tm_borders()

names(World)
class(World)
dplyr::glimpse(World)
World$continent

plot(World[1])
plot(World[,1])

plot(World[2,])

plot(World["pop_est"])

head(World[, 1:4])

class(World)


no_geom <- sf::st_drop_geometry(World)
class(no_geom)

#bounding boxes
st_bbox(World)

World %>%
  filter(continent == "Africa") %>%
  tm_shape() +
  tm_borders()

World %>%
  filter(continent == c("South America","Africa")) %>%
  tm_shape() +
  tm_borders()

#New variables in my map
World %>%
  mutate(our_countries =
           if_else(iso_a3 %in% c("ARG","COL","BRA", "MEX"),
                   "red", "grey")) %>%
  tm_shape(projection="+proj=robin") +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")

#Country polygons
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

# Shapefiles
dir.create("data/shapefiles", recursive = TRUE)

st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

bra2 <- read_sf("data/shapefiles/bra.shp")

class(bra)
plot(bra)

class(bra2)
plot(bra2)

# RASTER: Loading, plotting, and saving a raster

if (!dir.exists("data/raster"))
  dir.create("data/raster",
                                           recursive = TRUE)

# Getting raster from the worldclim
tmax_data <- getData(name = "worldclim",
                     var = "tmax",
                     res = 10,
                     path = "data/raster/")

plot(tmax_data)

is(tmax_data)
extent(tmax_data)
res(tmax_data)
