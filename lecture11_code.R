require(scatterplot3d)
require(RColorBrewer)
require(plot3D)
library(ggplot2)
library(mgcv)
library(visreg)
library(akima)
library(plotly)

# generate some random geospatial data:

x <- seq(from = -3, to = 3, by = 0.05)
y <- sin(x) + rnorm(length(x),0,0.5)
z <- sin(x) + sin(y) + rnorm(length(x),0,0.5)
# 2 d plot 
plot(x,y,col = 'grey')
points(x,sin(x),col='blue',type ='l',lwd = 2)

nclr <- 10 # number of colors
plotclr <- brewer.pal(nclr,"Spectral") # get the colors
colornum <- cut(rank(z), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatter plot
plot.angle <- 60
scatterplot3d(x,y,z, type="h", angle=plot.angle, color = colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray")



# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig


### Example of multivariate smoothing using additive models:

## INITIAL VALUES
norwaypaper.dat = read.table('datasets/norwaypaper.dat',header = TRUE)
y  = norwaypaper.dat$negy5
x1 = norwaypaper.dat$x1
x2 = norwaypaper.dat$x3
new.data<-data.frame(y=y,x1=x1,x2=x2)


nclr <- 7 # number of colors
plotclr <- brewer.pal(nclr,"BuPu") # get the colors
colornum <- cut(rank(y), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
plot.angle <- 60
scatterplot3d(x1,x2,y, type="h", angle=plot.angle, color = colcode, 
              col.axis="gray", col.grid="gray")

scatterplot3d(x1,x2,fitted(lmodel), type="h", angle=plot.angle, color = colcode, 
              col.axis="gray", col.grid="gray")

scatterplot3d(x1,x2,fitted(gamodel$fit), type="h", angle=plot.angle, color = colcode, 
              col.axis="gray", col.grid="gray")


## MAIN

## general linear model
lmodel  = lm(y~x1+x2, data = new.data)

## additive model with a smoothing function 
gamodel <-gam(y ~s(x1) + s(x2), data = new.data,family = 'gaussian', method = 'REML')

## OUTPUT
summary(lmodel)       # SUMMARY OF LINEAR MODEL FIT
summary(gamodel)      # SUMMARY OF GAM FIT

## OUTPUT PLOTS

## first,implement bivariate interpolation onto a grid for irregularly spaced input data
h.data = h.lm  = interp(x1,x2,y)
h.lm  = interp(x1,x2,fitted(lmodel))
h.gam = interp(x1,x2,fitted(gamodel))
#
fig.data <- plot_ly(x = h.lm$x, y = h.lm$y, z = h.data$z) %>% add_surface() %>% layout(title = "Data")

fig.lm <- plot_ly(x = h.lm$x, y = h.lm$y, z = h.lm$z) %>% add_surface() %>% layout(title = "Ordinary Linear Model")

fig.gam <-plot_ly(x = h.gam$x, y = h.gam$y, z = h.gam$z) %>% add_surface()%>% layout(title = "Generalized Additive Model")

## below are the plots:
fig.data

fig.lm

fig.gam


### VISUALIZING THE ADDITIVE EFFECT

par(mfrow = c(1,2))
visreg(gamodel, xvar = c("x1"),data = new.data,main = 'S(X1)')
visreg(gamodel, xvar = c("x2"),data = new.data, main = 'S(X2)')


par(mfrow = c(1,2))
visreg(lmodel, xvar = c("x1"),data = new.data,main = 'X1')
visreg(lmodel, xvar = c("x2"),data = new.data, main = 'X2')


### VISUALIZING THE ERROR TERM:
par(mfrow = c(1,2))
hist(h.lm$z - h.data$z,main = 'Difference between data \n and linear model fit')
hist(h.gam$z - h.data$z,main = 'Difference between data \n and additive model fit')



### DRUG ABUSE DATA EXAMPLE

## INITIAL VALUES
drugabuse.dat = read.table('datasets/drugabuse.dat',header=T)
y  = drugabuse.dat$drugfree
x1 = drugabuse.dat$numtreatments
x2 = drugabuse.dat$age


## visualizing the data:

par(mfrow = c(1,2))

ggplot(data = drugabuse.dat) + geom_density(aes(x=numtreatments, fill = as.factor(drugfree)), alpha = 0.5)

ggplot(data = drugabuse.dat) + geom_density(aes(x=age, fill = as.factor(drugfree)), alpha = 0.5)


## MAIN
gamodel.drug = gam(y~s(x1)+s(x2),family="binomial")

## OUTPUT
summary(gamodel.drug)      # SUMMARY OF MODEL FIT

## OUTPUT PLOTS
h.gam.drug = interp(x1,x2,fitted(gamodel.drug),duplicate="strip")
persp(h.gam.drug,theta=30,phi=15,ticktype="detailed",expand=0.5,
      xlab="x1",ylab="x2",zlab="P[drug-free]",
      main="Generalized Additive Model")
plot_ly(x = h.gam.drug$x, y = h.gam.drug$y, z = h.gam.drug$z) %>% add_surface()%>% layout(title = "Generalized Additive Model\n (Drug data example)")

par(mfrow = c(1,2))
visreg(gamodel.drug , xvar = c("x1"),data = drugabuse.dat,main = 'X1')
visreg(gamodel.drug , xvar = c("x2"),data = drugabuse.dat, main = 'X2')
