## simple illustration of bootstrap
## possible number of pseudo data:

theta <-c(3,4,5,6,8,9,10,13,14,18)
p <- c(1,3,3,1,3,6,3,3,3,1)

x.star<-c()
for (i in 1:1000){
  x.star0 <-sample(c(1,2,6), size = 3, replace = TRUE,  prob = c(1/3,1/3,1/3))
  x.star <- rbind(x.star0,x.star)
  
}

theta.star <- apply(x.star,1,mean)
theta.df <-data.frame(sample = round(theta.star,2))
plot(table(theta.df$sample)/1000,xlab = 'theta',ylab='estimated prob')