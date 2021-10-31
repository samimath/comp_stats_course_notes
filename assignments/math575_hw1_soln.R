## SOLUTION TO PROBLEM 3

## SETTING UP USEFUL FUNCTIONS


## LOGISTIC POPULATION GROWTH MODEL FOR BEETLES. It is a function of t and x (theta)
N.func <-function(t,x){
  N0 <-2
  out<-(N0*x[1])/(N0+(x[1]-N0)*exp(-x[2]*t))
  return(out)
}

## FIRST DERIVATIVES
N.prime <-function(t,x){
  ## 1 = k, 2 = r
  N0 <-2
  base <- N0+(x[1]-N0)*exp(-x[2]*t)
  out.1<- N0^2*(1-exp(-x[2]*t))/(base^2)
  out.2<- (x[1]*(x[1]-N0)*N0*t*exp(-x[2]*t))/(base^2)
  out = matrix(c(out.1,out.2),nrow=2)
  return(out)
}

## SECOND DERIVATIVES:
N.prime2 <-function(t,x){
  N0 <-2
  base <- N0 + (x[1]-N0)*exp(-x[2]*t)
  out.kk<- (-2)*(N0^2)*exp(-x[2]*t)*(1-exp(-x[2]*t))/(base^3)
  out.kr <- (N0^2*exp(-x[2]*t)*t)*(exp(-x[2]*t)*(N0-x[1])+2*x[1]-N0)/(base^3)
  out.rr<- (N0^2*exp(-x[2]*t)*t)*(N0*exp(-x[2]*t)-x[1]*exp(-x[2]*t)+2*x[1]-N0)/(base^3)
  out = matrix(c(out.kk,out.kr,out.kr,out.rr),nrow=4)
  return(out)
}

N.func <- Vectorize(N.func,'t')
N.prime <- Vectorize(N.prime,'t')
N.prime2 <- Vectorize(N.prime2,'t')

gamma <- function(x,t,y){

  out <- y - N.func(t,x)

  return(out)
}

## GAMMA.X is an n-by-1 vector measuring the difference between observed beetle count and the model. It is a function of x (theta) as the data is already embedded
gamma.x <- function(x){
  t = c(0,8,28,41,63,79,97,117,135,154)
  y=c(2,47,192,256,768,896,1120,896,1184,1024)
  out <- gamma(x,t,y)
  return(out)
}

N.prime.x <-function(x){
  t = c(0,8,28,41,63,79,97,117,135,154)
  out<-N.prime(t,x)
  return(out)
}


N.prime2.x <-function(x){
  t = c(0,8,28,41,63,79,97,117,135,154)
  out<-N.prime2(t,x)
  return(out)
}


g.prime <-function(x){

  g.prime.1 <-2*gamma.x(x)%*%N.prime.x(x)[1,]
  g.prime.2 <- 2*gamma.x(x)%*%N.prime.x(x)[2,]
  out <- matrix(c(g.prime.1,g.prime.2),nrow=2,byrow = TRUE)
  return(out)

}



g.prime.2 <-function(x){

  g.kk <-2*(gamma.x(x)%*%N.prime2.x(x)[1,] - (N.prime.x(x)[1,])%*%(N.prime.x(x)[1,]))
  g.kr <-2*((gamma.x(x)%*%N.prime2.x(x)[2,]) - (N.prime.x(x)[1,])%*%(N.prime.x(x)[2,]))
  g.rk <-g.kr
  g.rr <-2*((gamma.x(x)%*%N.prime2.x(x)[2,]) - (N.prime.x(x)[2,])%*%(N.prime.x(x)[2,]))
  out <- matrix(c(g.kk,g.kr,g.rk,g.rr),nrow=2, byrow=TRUE)
  return(out)

}



### PROBLEM 3.a 



t = c(0,8,28,41,63,79,97,117,135,154)
y=c(2,47,192,256,768,896,1120,896,1184,1024)
k0 = 1400
r0 = 0.1
x0 <-c(k0,r0)

print(paste('initial test: g(\theta) = ', g(x0)))

iter <-20
# initial array
#nr = Newton Raphson
#gn = Gauss Newton
x.vals.nr <-matrix(0,nrow = 2,ncol = iter+1)
x.vals.gn <-matrix(0,nrow = 2,ncol = iter+1)

x.vals.nr[1,1]<-x0[1]
x.vals.nr[2,1]<-x0[2]


x.vals.gn[1,1]<-x0[1]
x.vals.gn[2,1]<-x0[2]

## Newton-Raphson method:

for (i in 1:iter){
  ## Newton-Raphson:
  x.vals.nr[,i+1]<- x.vals.nr[,i]-solve(g.prime.2(x.vals.nr[,i]))%*%g.prime(x.vals.nr[,i])
  
  ## Gauss-Newton:
  A_i <-N.prime.x(x.vals.nr[,i])
  x.vals.gn[,i+1]<- x.vals.gn[,i]+solve(A_i%*%t(A_i))%*%A_i%*%(gamma.x(x.vals.gn[,i]))

}

par(mfrow = c(1,2))


plot(1:(iter+1), x.vals.gn[1,],type = 'b',col = 'blue',
     lwd = 2, xlim = c(1,iter+1),ylim = c(1000,1100),
     ylab = 'Estimate of K', xlab = 'iteration',main ='Gauss-Newton method')

plot(1:(iter+1), x.vals.nr[1,],type = 'b',col='forestgreen',
     lwd = 2, xlim = c(1,iter+1),ylim = c(1000,1100),
     ylab = 'Estimate of K', xlab = 'iteration', main ='Newton-Raphson method')




## contour plot:
x1 = seq(1000,1200,length=100)
x2 = seq(0.01,0.2,length=100)

g <-function(x){
  
  return(-1*gamma.x(x)%*%gamma.x(x))
  
}
z<-matrix(0,ncol = 100,nrow = 100)
for(i in 1:100){
  for(j in 1:100){
    z[i,j] = g(c(x1[i],x2[j]))
  }
}

contour(x1,x2,z,nlevels=50)
