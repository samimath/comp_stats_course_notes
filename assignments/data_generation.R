A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
                             14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
                    Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
                               46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
                               22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), .Names = c("Time", "Counts"), row.names = c(1L, 2L,
                                                                                                                                        3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L,

### radio data

                                                                                                                                                                                                                                                                                31L), class = "data.frame")
n <- 30
exp.rand <-rexp(n = n, rate = 4)
noise <- rnorm(n,0,0.01)
exp.data <- exp.rand + abs(noise)

hist(exp.data)
print(mean(exp.data))

exp.density <- function(lambda,x){
  if(x>0){
   return(lappllambda*exp(-lambda*x))  
  }
  else{
    return(0)
  }
}

exp.lik <-function(par,x = exp.data){
  
  ## par = vector of parameters. In this case, it has only one value
  
  ## x = the data we are evaluating MLE for. The default is set up exp.data which is the variable name I gave to the dataset
  
  ## n =. should be the length of the dataset 
  n <- #length(x)
  
  ##. ll should be the log-likelihood function, in here expressed as par, n,  and x
  ll <- #n*log(par[1]) - par[1]*sum(x)
  
  return(ll)
  
}


optim(par = c(1), 
      fn = exp.lik, 
      method = 'L-BFGS-B', 
      control =list(trace=1,maxit=1000,fnscale = -1),
      hessian = TRUE)