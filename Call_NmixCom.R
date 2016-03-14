setwd("C:/NmixCom")

library(R2WinBUGS)

# bundle data
y <- as.matrix(read.csv("Data_LowerRegionAbund.csv", header=T, row.names=1)) # fish data
t <- c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41) 
# Lower sites combined (Seneca, Whites Ferry, Brunswick): sample years from 1975 (year=1); note absence of data in 1985 and 1986

S <- nrow(y) # number of species
T <- ncol(y) # number of years
Q <- read.csv("Data_AnnualQ.csv", head=T, sep=",")
Q <- subset(Q, Year != "1985" & Year != "1986") # removing years lacking data
Q <- as.vector(scale(Q[,2]))

# load data
win.data <- list(S=S,T=T,t=t,y=y,Q=Q)

# initial values
inits <- function() {list(beta=dnorm(0,0.0001), sigma=dnorm(0,0.0001))}

# parameters to estimate
parameters <- c("r","b")

# MCMC settings
ni <- 10000
nb <- 1000
nt <- 5
nc <- 3

# Gibbs sampling
out <- bugs(win.data, inits, parameters, model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, 
            debug=T, working.directory=getwd(), bugs.directory="C:/WinBUGS14")
save(out, file="Out_Lower.RData")

################################
# inspect model
hist(out$summary[,8], col="black", xlab="Rhat")

# plot chains
par(mfrow=c(3,1)) 
matplot(out$sims.array[,,1],type="l") # intercept for species 1
matplot(out$sims.array[,,2],type="l") # intercept for species 2
matplot(out$sims.array[,,3],type="l") # etc