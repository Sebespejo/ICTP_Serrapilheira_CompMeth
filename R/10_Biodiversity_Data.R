install.packages("rgbif")
install.packages("Taxonstand")
install.packages("CoordinateCleaner")
install.packages("maps")

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)

library(rgbif)
library(dplyr)
species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species,
                   limit = 100000)
names(occs)

#Let's create a new object from the table
myrsine.data <- occs$data

colnames(myrsine.data)

#Exporting raw data
dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,"data/raw/myrsine_data.csv",
          row.names = FALSE)


#Checking species taxonomy
sort(unique(myrsine.data$scientificName))

#Names accepted or not?
table(myrsine.data$scientificName, myrsine.data$taxonomicStatus)


#Nomenclature Checking
species.names <- unique(myrsine.data$scientificName)
dim(species.names)

tax.check <- TPL(species.names)
tax.check

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax,
                         by = "scientificName")

#or in dplyr
#myrs_tax <- left_join(myrsine.data,new.tax)
#dim(myrs_tax)

#Exporting data after taxonomy checked
dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)

#Checking species coordinates
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)

#Cleaning species record
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]


# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")

##Map with clean coordinates
par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
map(, , , add = TRUE)

par(mfrow = c(1, 1))



myrsine.new.geo <- clean_coordinates(x = myrsine.coord,
                                     lon = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     species = "species",
                                     value = "spatialvalid")

# merging w/ original data
myrsine.new.geo2 <- merge(myrsine.data, myrsine.new.geo,
                          all.x = TRUE,
                          by = "key")
plot(decimalLatitude ~ decimalLongitude, data = myrsine.new.geo, asp = 1)
map(, , , add = TRUE)

#Exportinf data after coordinate checking
write.csv(myrsine.new.geo2,
          "data/processed/myrsine_coordinate_check.csv",
          row.names = FALSE)



######How to do this map in tmap?
##1) create a SF object
##2) tmap_mode("view")
