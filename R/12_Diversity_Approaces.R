#functions of diversity

##Example of diversity measurements in two communities

library(vegan)
Community.A <- c(10,6,4,1)
Community.B <- c(17, rep(1,7))

diversity(Community.A,"shannon")
diversity(Community.B,"shannon")
diversity(Community.A,"invsimpson")
diversity(Community.B,"invsimpson")

renyiA <- renyi(Community.A)
renyiB <- renyi(Community.B)

ren_AB <- rbind(renyiA,renyiB)
matplot(t(ren_AB),type = "l",axes = F)
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "inf"),
     at = 1:11)
legend("topright", legend = c("Community A","Community B"),
       lty = c(1, 2), col = c(1,2))

ren_comB_Hill <- renyi(Community.B, hill = T)
