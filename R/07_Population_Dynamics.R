##Population dynamics

library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

source("fct/logGrowth.R")
source("fct/LVComp.R")


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
#you can also create sequences like seq(1,200,by=.1)

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

#MacArthurModel
RwMcPred <- function(t, y, p) {
  N <- y[1] ; P <- y[2]
  with(as.list(p), {
    dN.dt <- (r1 * N * (1-N/K)) - ((c*N*P)/(1+c*tau*N))
    dP.dt <- ((b * N * P) / (1+c*tau*N))- d * P
    return(list(c(dN.dt, dP.dt)))
  })
}

# RwMc parameters
c <- 2 #PconsumptionRate
b <- 1 #c convertion coef
tau <- 0.2 #manipulationTime
d <- 0.2 #P deathRate
a1 <- list(c(c,b,tau,d))
# here we have to add the predation coefficients, c, b, tau and d which are consumption, converted consumption and manipulationTime and PdeathRate respectively
r1 <- 10 # growthRate N
p3 <- list(r1, a1)
N1 <- c(5000, 100)
t2 <- c(1:1000)
K <- 15000

out_RwMc <- ode(y = N1, times = t2, func = RwMcPred, parms = p3)

#head(out_RwMc)

df_RwMc <- pivot_longer(as.data.frame(out_RwMc), cols = 2:3)

#Plot
ggplot(tb_RwMc) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()


