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


set.seed(1)
# generate uniform random numbers
x <- runif(20)
par(mfrow = c(2,2))
hist(x)
# fit different types of kernel function
kde <- density(x, kernel = "gaussian",bw = 0.05)
plot(kde)
kde1 <- density(x, kernel = "gaussian",bw = 0.1)
plot(kde1)
kde2 <- density(x, kernel = "gaussian",bw = 0.5)
plot(kde2)



### finding optimal h using cross-validation:
kdenorm <- function(x,bw,q=NULL){
  if(is.null(q)) {
    q = seq(min(x)-3*bw, max(x)+3*bw, length.out=512)
  }
  
  nx = length(x)
  print(nx)
  nq = length(q)
  print(nq)
  
  # define X_i - x
  xmat = matrix(q,nq,nx) - matrix(x,nq,nx,byrow=TRUE)
  # define kernel function
  denall = dnorm(xmat/bw) / bw
  # define f hat
  denhat = apply(denall,1,mean)
  list(x=q, y=denhat, bw=bw)
}

intfun <- function(ix,x,bw){
  
  kdenorm(x = x,bw = bw,q=ix)$y^2
  
  }

### Defined unbiazed cross validation (UCV) criterion
kdecv <- function(bw,x){
  lo = min(x)-3*bw
  up = max(x)+3*bw
  ival = integrate(intfun,x=x,bw=bw,lower=lo,upper=up)$value
  nx = length(x)
  ival - (2/(nx-1))*sum( kdenorm(x,bw,x)$y - dnorm(0)/(nx*bw) )
}

### test it with an example:
set.seed(1)
x = (1/3)*rnorm(100,mean = 4,sd = 3)+(2/3)*rnorm(100,mean = 10,sd = 6)
opt.bw <-optimize(kdecv,interval=c(0.05,1),x=x)$minimum

hist(x,freq = FALSE,col = 'lightgray')
lines(density(x, kernel = "gaussian",bw = opt.bw),col='blue',lwd = 2)


