#Can we use the centroid size and baseline size to predict the Procrustes distance

#Install and load the shapes package
install.packages("shapes")
library(shapes)

#Load the raw data in from the txt file
setwd("/Volumes/My Passport for Mac/Final Year/Statistical Shape & Image Analysis/Coursework 1")
ImportData = read.table("data1.txt",header=FALSE ,sep="")

#Format the imported data as an array
k <- 10
m <- 2
n <- 30
data <- array(0,c(k,m,n))
data[,1,] <- array(ImportData$V1,dim=c(k,1,n))
data[,2,] <- array(ImportData$V2,dim=c(k,1,n))

#Remove outlier shape
data <- data[,,2:30]
n <- 29

#Calculate mean shape
pms <- procGPA(data,eigen2d=FALSE)$mshape
#Calculate Riemannian distance to mean shape
p_dist <- numeric()
for  (i in 1:(n)){
  p_dist[i] <- riemdist(data[,,i],pms)
}


#Calculate sizes
csize <- centroid.size(data)
bsize <- sqrt((data[10,1,]-data[1,1,])^2 + (data[10,2,]-data[10,2,])^2)
c_over_b <- csize/bsize


# Plot the distance vs the size measures and calculate correlation coefficients
par(mfrow = c(2,2))
plot(p_dist,csize,xlab="Distance to mean shape",ylab="Centroid size")
plot(p_dist,bsize,xlab="Distance to mean shape",ylab="Baseline size")
plot(p_dist,c_over_b,xlab="Distance to mean shape",ylab="Size ratio")
plot(p_dist,abs(c_over_b-median(c_over_b)),xlab="Distance to mean shape",ylab="||Size ratio minus average||")
cor(p_dist,csize)
cor(p_dist,bsize)
cor(p_dist,c_over_b)
cor(p_dist,c_over_b-median(c_over_b))
cor(p_dist,abs(c_over_b-median(c_over_b)))


# Generate and analyse a linear model
lm <- lm(p_dist~abs(c_over_b-median(c_over_b)))
qqnorm(lm$residuals)
summary(lm)
shapiro.test(lm$residuals)

#Time measurements
date()
for (i in 1:1000){
  pms <- procGPA(data,eigen2d=FALSE)$mshape
  #Calculate Riemannian distance to mean shape
  p_dist <- numeric()
  for  (i in 1:(n)){
    p_dist[i] <- riemdist(data[,,i],pms)
  }
}
date()

est_dist = numeric()
#Size method
date()
for (i in 1:1000){csize <- centroid.size(data)
bsize <- sqrt((data[10,1,]-data[1,1,])^2 + (data[10,2,]-data[10,2,])^2)
c_over_b <- csize/bsize
for(i in 1:n){
  est_dist[i] <- 0.018765 + 0.960718*abs(c_over_b[i]-median(c_over_b))
}
}
date()
