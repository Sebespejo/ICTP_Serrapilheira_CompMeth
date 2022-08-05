#From Taxonomical to Funcional and Phylogenetic Diversity

library(vegan)
library(taxize)
library(dplyr)

comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")
splist <- read.csv("data/raw/cestes/splist.csv")

head(traits)[,1:6]
rownames(comm)[1:6]

rownames(comm) <- paste0("Site", comm[, 1])
comm <- comm[,-1]
head(comm)[,1:6]

head(traits)[,1:8]
View(traits)

#Changing DF row names
rownames(traits) <- t(traits[1])
traits <- subset(traits, select = -1)
head(traits)[,1:8]


richness <- vegan::specnumber(comm)
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")

#Functional Diversity}
library(cluster)
library(FD)
gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?


class(gow) #different classes

plot(gow, gow2, asp = 1) #same values


install.packages("SYNCSA")
library(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)


#install.packages("FD")
library(FD)
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)

FuncDiv <- dbFD(x = traits, a = comm, messages = F) #plotear esto

#Check species family
classification_data <- classification(splist$TaxonName,
                                      db = "ncbi")


#Extract families using a function
extract_family <- function(x) {
  if (!is.null(dim(x) )){
    y <- x %>%
      filter(rank == "family") %>%
      pull(name)
    return(y)
  }
}

#Extract families
extract_family(classification_data[[1]])

#Families list
families <- vector()
for (i in 1:length(classification_data)){
  f <- extract_family(classification_data[[i]])
  if (length(f) > 0) families[i] <- f
}

#Check Susana Magallán: metacalibrated trees

