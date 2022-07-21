##Population dynamics

library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

#source("fct/logGrowth.R")
#source("fct/LVComp.R")


#Logistic model
## Creating a function for logistic growth
logGrowth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - a * N)
    return(list(dN.dt))
  })
}


## named vector with parameters
p <- c(r = 0.1, a = 0.05)
## initial condition
y0 <- c(N = 10)
## time steps
t <- 1:200
  #you can alsoo create sequences like seq(1,200,by=.1)

## give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)


##Plotting the result
df_log <- as.data.frame(out_log) #as.data.frame is used to create a new data frame
ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()

#########################################

#Lotka-Volterra Competition Model
##Creating the function
LVComp <- function(t, y, p) {
  N <- y
  with(as.list(p), {
    dN1.dt <- r[1] * N[1] * (1 - a[1, 1] * N[1] - a[1, 2] * N[2])
    dN2.dt <- r[2] * N[2] * (1 - a[2, 1] * N[1] - a[2, 2] * N[2])
    return(list(c(dN1.dt, dN2.dt)))
  })
}

##LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 100)
t2 <- c(1:100)

##Solving the system of ODEs
out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)

##Plotting
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()

