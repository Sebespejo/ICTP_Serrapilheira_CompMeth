#Multivariate Analysis

library(vegan)
#install.packages("cluster")
library(cluster)

data("dune")
data(dune.env)
table(dune.env$Management)


bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))
#

##Clustering method for the analysis: average
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

###Plot
par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)




par(mfrow = c(1, 1))


par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1),
     las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1),
     las = 2)

#Redundancty analysis
is(chord_distance)
#We standardize community data with function decostand
norm <- decostand(dune, "norm")
#rda is the constrained version of PCA
pca <- rda(norm)

plot(pca)
summary(pca)
dim(dca)

plot(pca,choice = c(2,3))

names(dune.env)
apply(dune.env, 2, class)
#all the variables are characters
library(dplyr)
dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)

pca_env <- rda(dune.env[, c("A1", "Moisture",
                            "Manure")])
#recoding
plot(pca, choices = c(2, 3))
plot(pca_env)
cor(pca_env)
