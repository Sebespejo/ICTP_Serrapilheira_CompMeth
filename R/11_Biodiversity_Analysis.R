##Importing data to do analysis

library(dplyr)
library(readr)

comm <- read.csv("data/raw/cestes/comm.csv")
View(comm[-1])

#Abundance in order
abundance <- sort(colSums(comm[,-1]),decreasing = TRUE)
View(abundance)
max(unlist(abundance))

#Binary matrix if x>0 => x=1
binarycomm <- rowSums(sign(comm[,-1]))
View(binarycomm)

#Now, count richness
richness <- rowSums(binarycomm)

#Abundance of the most abundant species per site
mostabpersite <- apply(comm[-1],MARGIN = 1,FUN = max)

#Most abundant species per site
spabpersite <- apply(comm[-1],MARGIN = 1,FUN = which.max)
print(spabpersite,mostabpersite)

##Create an element showing the most abundant species
##and its abundance
mapply(c,spabpersite,mostabpersite)


#Diversity indeces
##Relative diversity
relatabund <- apply(abundance)


