### R warm up:

##How to define a vector, a matrix, a dataframe, or a list in R?
v <-c(1,3,5,7,9)

M <-matrix(cbind(v,2*v), ncol = 2)

df <-data.frame(c1=c(1,2,3),c2=c('a','b','c'),
                c3=c(1.1e2,3e2,4.7e3),c4 =c(TRUE,FALSE,NA))
s <-list(df=df,M=M,v=v,k=3)



##What are the differences among the above objects?
  

## How to define functions in R?

hello.world <-function(greeting=NA,print=FALSE){
  
  if(toupper(greeting) %in% c('HI','HELLO')){
    
    message <-'Hello to you too'
    
  }
  
  else{ message <-'Try again!'}
  
  if(print){
    
    print(message)
  }
  return(message)
  
}


##How to do matrix operations in R?

## matrix mutliplecation:

M%*%t(M)

# get matrix inverse:
A <-matrix(c(2,1,4,5),ncol=2,nrow = 2)
A.inv <-solve(A)

# solve for a matrix equation in the form of Ax = b
b <- c(3,1)
x <- solve(A,b)
# check answer:
b.solve <- A%*%x

sum((b-b.solve))^2

# get eigen value and eigen vector Av = lambdav (returns a list)
eigen.out <- eigen(A)

  
  