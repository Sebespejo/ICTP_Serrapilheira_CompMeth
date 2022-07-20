##Model selection

library(bbmle)
cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")



h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))

AICtab(h0,h1,h2,h3)
AICtab(h0,h1,h2,h3,)


#Predicted values------
##Calculating the predicted values
newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species))
newdata$Beg <- predict(cuckoo_glm, newdata, type = 'response')


## explore ?predict.glm

p <- ggplot(mapping = aes(x = Mass, y = Beg, colour = Species)) +
  geom_point(data = cuckoo) +  geom_line(data = newdata) +
  mtheme_classic()
