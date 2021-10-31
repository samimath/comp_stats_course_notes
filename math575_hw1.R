library(matlib)
library(pracma)
library(MASS)
install.packages('rmarkdown')


#1

#1a.
#claims.csv was downloaded and imported as a dataset
claims=claims[1]
mle=mean(claims$x)
mle

#1c.
#mle=42.61154

#1d.
set.seed(10)
lambda=mle
random_var <- rexp(length(claims), 1/lambda)
random_var
library("car")
qqPlot(random_var)
qqPlot(claims)
#the qqplots both show that the greatest density is 
#nearest the value of the mle for both datasets.


#2
x<-c(3.91, 4.85, 2.28,4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
mean(x)
domain <-seq(from = -pi, to = pi, by = pi/180)
sum=0

range=1:length(x)
plot(domain,lapply(domain,function(theta){
  
    for (i in range){
      sum=sum+log((1-cos(x[i]-theta))/(2*pi))
    }
    sum

}
),main="likelihood plot")
theta=0




theta=0.0584

dl<-function(theta){
  result=0
  for(i in range){
    result=result+((sin(x[i]-theta))/((1-cos(x[i]-theta))))
  }
  result*-1
}
dll<-function(theta){
  result=0
  for(i in range){
    result=result-(cos(x[i]-theta)*(cos(x[i]-theta)-1)+((sin(x[i]-theta)**2)))/((1-cos(x[i]-theta))**2)
  }
  result
}

#Newton raphson with theta=mle=0.0584
for(i in 1:100000){
  theta=theta-(dl(theta)/(dll(theta)))
}
theta
#-0.011972 is predicted value of theta

#Newton raphson with theta=-2.7
theta=-2.7
for(i in 1:10000){
  theta=theta-dl(theta)/(dll(theta))
  print(dl(theta)/(dll(theta)))
}
theta
#-2.6667 is predicted value

#Newton raphson with theta=+2.7
theta=2.7
print(dl(-2.5))
print(dl(0.05))
for(i in 1:10000){
  theta=theta-dl(theta)/(dll(theta))
}
theta
#2.873095 is predicted value



ff<-function(theta){
  
  for (i in range){
    sum=sum+log((1-cos(x[i]-theta))/(2*pi))
  }
  sum
  
}
y<-seq(from =-pi,to =pi,by =pi/100)
modes=c()
for(i in 1:length(y)){
  theta=y[i]
  for(j in range){
  theta=theta-dl(theta)/(dll(theta))
  }
  print(theta)
  if(i==1){
    modes=append(modes,theta)
  }
  if(modes[length(modes)]!=theta){
    print("hi")
    modes=append(modes,theta,after=length(modes))
    
  }
}
modes
emp=c()
for(item in range){
  append(emp,1)
}

#f' =sin(x-&)/2pi
#f''=cos(x-&)/2pi
#3.
#below are the initial guesses
k=5000
n=15
r=59
beetles <- c(2,47,192,256,768,896,1120,896,1184,1024)
t <- c(0,8,28,41,63,79,97,117,135,154)
theta<-matrix(c(r,k,n),nrow=3,ncol=1)
f<-function(r,n,k,t){
  (k*n)/(n+((k-n)*exp(-r*t)))
}

fk<-function(r,n,k,t){
  (n*((n+(k-n)*exp(-r*t)))-k*n*(exp(-r*t)))/((n+(k-n)*exp(-r*t))**2)
}#g
fr<-function(r,n,k,t){
  (k*n*t*(k-n)*exp(-r*t))/((n+(k-n)*exp(-r*t))**2)
}#g
fn<-function(r,n,k,t){
  (k*(n+(k-n)*exp(-r*t))-(k*n)*(1-exp(-r*t)))/((n+(k-n)*exp(-r*t))**2)
}#g
ft<-function(r,n,k,t){
  ((k-n)*(r*(k-n)*exp(-r*t)))/((n+(k-n)*exp(-r*t))**2)
  
}
print(1)
#theta[1,1]=k
#A<-matrix(c(fk(theta[2,1],theta[1,1],n),fn(theta[2,1],n,theta[1,1])),nrow=2,ncol=1)
#A
#A

#for(k in 1:length(beetles)){
#  X[k,1]=beetles[k]-
#  
#}


#theta2=k
#theta1=r

Xcalc<-function(r,n,k){
X<-matrix(0,nrow = 10,ncol = 1)
  
for (j in 1:length(beetles)-1){
  
  X[j,1]=beetles[j]-((k*n)/(n+((k-n)*exp(-r*t[j]))))
}
X
}
A<-matrix(0,nrow=10,ncol=3)
sims=1:1000
for (k in sims){
  #inv(t(A)%*%A)%a*%t(A)%*%X
  X<-Xcalc(theta[1,1],theta[3,1],theta[2,1])
  for(z in 1:10){
    for(j in 1:3){
      if(j==1){
      A[z,j]=fr(theta[1,1],theta[3,1],theta[2,1],t[z])
     
      }
      if(j==2){
        A[z,j]=fk(theta[1,1],theta[3,1],theta[2,1],t[z])
       
      }
      if(j==3){
        A[z,j]=fn(theta[1,1],theta[3,1],theta[2,1],t[z])
        
      }
      }
    }
  #A<-matrix(c(fk(theta[2,1],theta[1,1],n),fn(theta[2,1],n,theta[1,1])),nrow=2,ncol=1)
  print(theta)
#X=10*1. #pinv(t(A))=10*2
  
  theta=theta+pinv((A))%*%(X)
}
#(n*((n+(k-n)*exp(-r*t)))+k*n*(exp(-r*t)))/((n+(k-n)*exp(-r*t))**2)
fkk<-function(r,n,k,t){
  ((n*exp(-r*t)-k*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*exp(-r*t)*(n*(n+(k-n)*exp(-r*t))-k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}#g
fkn<-function(r,n,k,t){
  ((2*n+k*exp(-r*t)-2*n*exp(-r*t)+k*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*(1-exp(-r*t))*(n*(n+(k-n)*exp(-r*t))+k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}
fnk<-function(r,n,k,t){
  ((n+2*k*exp(-r*t)-2*n*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*exp(-r*t)*(k*(n+(k-n)*exp(-r*t))-k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}
fnn<-function(r,n,k,t){
  ((2*n+k*exp(-r*t)-2*n*exp(-r*t)-k*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*(1-exp(-r*t))*(n*(n+(k-n)*exp(-r*t))-k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}#g
#fnn<-function(r,n,k,t){
#  ((k-k*exp(-r*t)-k*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(1-exp(-r*t))*(k*(n+(k-n)*exp(-r*t)))-k*n*exp(-r*t))/((n+(n-k)*exp(-r*t))**4)
#}
frr<-function(r,n,k,t){
  (-1*(t**2)*k*n*(k-n)*exp(-r*t)*((n+(k-n)*exp(-r*t))**2)+2*t*(n+(k-n)*exp(-r*t))*k*n*t*(n-k)*exp(-r*t))/((n+(k-n)*exp(-r*t))**4)
}#g

frn<-function(r,n,k,t){
  ((2*k*n*t*exp(-r*t)-(k**2)*t*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*(1-exp(-r*t))*(k*n*t*(n-k)*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}
frk<-function(r,n,k,t){
  (((n**2)+exp(-r*t)-2*k*n*t*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*exp(-r*t)*((n**2)*t*exp(-r*t)-2*k*n*t*exp(-r*t)))/((n+(k-n)*exp(-r*t))**2)
}
fkr<-function(r,n,k,t){
  ((-t*n*k*exp(-r*t)+t*(n**2)*exp(-r*t)-t*k*n*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*(-t*(k-n)*exp(-r*t))*(n*(n+(k-n)*exp(-r*t))+k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}

fnr<-function(r,n,k,t){
  ((k*t*(n-k)*exp(-r*t)+k*n*t*exp(-r*t))*((n+(k-n)*exp(-r*t))**2)-2*(n+(k-n)*exp(-r*t))*-t*(k*(n+(k-n)*exp(-r*t))-k*n*exp(-r*t)))/((n+(k-n)*exp(-r*t))**4)
}#g
#Newton Raphson
hess=matrix(0,nrow=3,ncol=3)
gradvec=matrix(0,nrow=3,ncol=1)
#hessian for approximation is 3x3
#i think for theta estimation, it's 3n x3
#theta is 3x1
#f' matrix would be 3nx1
#J*f'=3x3 ^T *3n*1 = 3*1
gradcalc<-function(r,n,k,t){

    gradvec[1]=fk(theta[1,1],theta[3,1],theta[2,1],t)
    gradvec[2]=fr(theta[1,1],theta[3,1],theta[2,1],t)
    gradvec[3]=fn(theta[1,1],theta[3,1],theta[2,1],t)
  
  
}
hesscalc<-function(){

  
  
  hess[1,1]=fkk(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[1,2]=fkr(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[1,3]=fkn(theta[1,1],theta[3,1],theta[2,1],t[z])
  
  hess[2,1]=frk(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[2,2]=frr(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[2,3]=frn(theta[1,1],theta[3,1],theta[2,1],t[z])
  
  hess[3,1]=fnk(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[3,2]=fnr(theta[1,1],theta[3,1],theta[2,1],t[z])
  hess[3,3]=fnn(theta[1,1],theta[3,1],theta[2,1],t[z])
  
  hess
  #theta=theta+
}
#2x2 matrix for a single point 
xi=2

for(m in sims){
  hess<hesscalc()
  gradvec<gradcalc(theta[1,1],theta[3,1],theta[2,1],xi)
  xi=xi+inv(t(hess))*gradvec
}
theta
#
#
#pinv(A)=2x10
#
#2*1, 10*1
#A=2x1.
#X=10*1
print(Ginv(A))
print(pinv(t(A)))#=2x10
print(X)#10X1
#A=2x1.A^T=1X2. A&A^T=1*1 . A*A^T&A^T= 1*2
t(A)*A
theta
  
