
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
  ival - (2/(nx-1))*sum(kdenorm(x,bw,x)$y - dnorm(0)/(nx*bw) )
}

### test it with an example:
set.seed(1)
### simulate data:
x = (1/3)*rnorm(100,mean = 4,sd = 3)+(2/3)*rnorm(100,mean = 10,sd = 6)
opt.bw <-optimize(kdecv,interval=c(0.05,10),x=x)$minimum

hist(x,freq = FALSE,col = 'lightgray',breaks = 10)
lines(density(x, kernel = "gaussian",bw = opt.bw),col='blue',lwd = 2)


