####################################################################################
## R example: simple example comparing MC simulation and importance sampling
####################################################################################


## number of dice to roll
n<-200

## function to simulate our event of interest
dice.roll.f <-function(n){
  for (i in 1:n){
    # sample uniformly over 1 to 6
    s <- c(s,sample(c(1,2,3,4),1))
  }
  exp.val <-sum(s==1)/n
  return(exp.val)
}

## modified sampling function with an oversampling step, and a correction step
dice.roll.g <-function(n){
  
  for (i in 1:n){
    ## over sample
    s <- c(s,sample(c(1,2,3,4),1,prob = c(0.5,0,0,0.5)))
  }
  ## correct by importance ratio
  exp.val <-(1/2)*sum(s==1)/n
  return(exp.val)
}
## vectorize the functions:
dice.roll.f<-Vectorize(dice.roll.f,'n')
dice.roll.g<-Vectorize(dice.roll.g,'n')

## evaluate the two sampling schemes over a series of iterations
ns <-seq(from = 10,to = 2500, by = 10)
f.est <-dice.roll.f(ns) 
g.est <-dice.roll.g(ns) 
## plot the coefficient of variation:
#plot(ns, sqrt(5/nsl),type = 'l',
#     xlab = 'Number of samples', ylab = 'CV',lwd = 3,
#     ylim = c(0,0.3))
#abline(h = 0.05, col = 'red',lwd = 3,lty = 2)
#text(1900,0.1,"5% CV",col='red',lwd =3)
sample.points<-c(20,100,500,1000,1500,2000,2500,3000)
val<-matrix(ncol = length(sample.points),nrow = 100)
for(k in 1:100){
  for(i in 1:length(sample.points)){
    val[k,i]<-dice.roll.f(sample.points[i])  
  }
}

par(mfrow = c(2,4))
for(i in 1:length(sample.points)){
  hist(val[,i],xlim = c(0.15,0.4),
       main = paste(sample.points[i],'samples'),
       col = '#A93226',xlab = 'E(X)')
}
## visualize the result
par(mfrow = c(1,2))
plot(ns,f.est,type ='b',col = 'blue',ylim = c(0.15,0.3),
     main = 'Naive Monte Carlo simulation',
     xlab = 'Iter', ylab ='Estimate')
#abline(h=0.156, col = 'red',lwd = 2,lty = 2)
#abline(h=1/6, col = 'red',lwd = 2)
#abline(h=0.176, col = 'red',lwd = 2,lty = 2)

plot(ns,g.est,type ='b',col = 'forestgreen',ylim = c(0.1,0.3),
     main = 'Monte Carolo simulation \nwith importance sampling',
     xlab = 'Iter', ylab ='Estimate')
#abline(h=0.156, col = 'red',lwd = 2,lty = 2)
#abline(h=1/6, col = 'red',lwd = 2)
#abline(h=0.176, col = 'red',lwd = 2,lty = 2)


