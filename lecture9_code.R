## Kernel density function examples

## Gausian

K.gaussian <- function(u){
  (1/sqrt(2*pi))*exp(-u^2/2)
}
K.gaussian <-Vectorize(K.gaussian,'u')


## Sigmoid

K.sigmoid <- function(u){
  (2/pi)*(1/(exp(u)+exp(-u)))
}

## Logistic

K.logistic<- function(u){
  1/(exp(u)+exp(-u)+2)
}

u<-seq(-3,3,by = 0.01)

plot(u,K.gaussian(u),type = 'l',col ='blue',
     lwd = 2, ylab = 'K(u)', xlab = 'u',main = '4 types of Kernel')
lines(u,K.sigmoid(u),col = 'red',lwd = 2)
lines(u,K.logistic(u),col = 'forestgreen', lwd =2)
lines(u,rep(1/6,length(u)),col = 'black',lwd=2)

legend(1.5,0.3,legend = c('Gaussian','Sigmoid','Logistic','Uniform'),
       col = c('blue','red','forestgreen','black'),lty = c(2,2,2,2), 
       cex=.8)


