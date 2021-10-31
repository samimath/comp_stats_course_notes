## Mixture data

## READ IN `mxture.dat` in the attachment
mixture.dat = read.table(file.choose(),header=TRUE)
y = mixture.dat$y

## HISTOGRAM OF DATA AND PLOT OF MIXTURE DISTRIBUTION (See figure 7.1)
par(mfrow=c(1,1))
x=seq(5,14,by=.01)
d=.7*dnorm(x,7,.5) + .3*dnorm(x,10,.5)
hist(y,breaks=20,freq=FALSE,
     main="Histogram of mixture data \n See Fig 7.1 in Givens and Hoeting",
     ylab="Density")
points(x,d,type="l")

## INITIAL VALUES
n = 10000
x.val1 = NULL
x.val2 = NULL
set.seed(0)

## FUNCTIONS
# BETA(1,1) PROPOSAL DENSITY
g = function(x){dbeta(x,1,1)}
# TARGET DENSITY FUNCTION 
f = function(x){prod(x*dnorm(y,7,0.5) + (1-x)*dnorm(y,10,0.5))}
# M-H RATIO
R = function(xt,x){f(x)*g(xt)/(f(xt)*g(x))}

## MAIN

# INITIAL VALUE : sample from Beta(1,1)
x.val1[1] = rbeta(1,1,1)

# M-H algorithm
for(i in 1:n){
  # start value for X
  xt = x.val1[i]
  # sample from proposal function
  x = rbeta(1,1,1)
  # probability of acceptance
  p = min(R(xt,x),1)
  # decide on acceptance based on p
  d = rbinom(1,1,p)
  # update X 
  x.val1[i+1] = x*d + xt*(1-d)
}
mean(x.val1[201:(n+1)])
par(mfrow=c(1,2))
plot(x.val1[1:(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title("Sample path for Beta(1,1) Proposal Dist.")
hist(x.val1[201:(n+1)],breaks=20,xlab="delta",
     main="Hist. for Beta(1,1) Proposal Dist.")

# BETA(2,10) PROPOSAL DENSITY
g = function(x){dbeta(x,2,10)}
x.val2[1] = rbeta(1,2,10)
for(i in 1:n){
  xt = x.val2[i]
  x = rbeta(1,2,10)
  p = min(R(xt,x),1)
  d = rbinom(1,1,p)
  x.val2[i+1] = x*d + xt*(1-d)
}
mean(x.val2[201:(n+1)])
plot(x.val2[201:(n+1)],ylim=c(0,1),type="l",ylab="delta",xlab="t")
title("Sample path for Beta(2,10) Proposal Dist.")
hist(x.val2[201:(n+1)],breaks=20,xlab="delta",
     main="Hist. for Beta(2,10) Proposal Dist.")

par(mfrow=c(1,1))
d.est <- mean(x.val1)
y.est <-d.est*dnorm(x,7,.5) + (1-d.est)*dnorm(x,10,.5)
hist(y,breaks=20,freq=FALSE,
     main="Histogram of mixture data \n See Fig 7.1 in Givens and Hoeting",
     ylab="Density")
points(x,d,type="l")


par(mfrow=c(1,1))
x=seq(5,14,by=.01)
y.est <-d.est*dnorm(x,7,.5) + (1-d.est)*dnorm(x,10,.5)
hist(y,breaks=20,freq=FALSE,
     main="Histogram of mixture data \n Comparison of model parametere estmiates",
     ylab="Density")
points(x,d,type="l",lwd = 2)
points(x,y.est,type="l",col='blue',lwd=2,lty =2)
legend(9,0.6,c('orginal densty','M-H estimate'),col=c('black','blue'),pch =c(15,15))