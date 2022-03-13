# cran package: https://cran.r-project.org/web/packages/Barycenter/index.html
library(Barycenter)
library(Rcpp)

# cran package: https://cran.r-project.org/web/packages/HistDAWass/index.html
#install.packages("HistDAWass", dep=TRUE)
library(HistDAWass)
# description / documentation of HistDAWass package:
# https://www.rdocumentation.org/packages/HistDAWass/versions/1.0.6











#Try

# this is andrea; lucy type below
# lucy types here






install.packages("Barycenter", dep=TRUE)
#update.packages('Rcpp')
update.packages('Barycenter')
install.packages("Rcpp", dep=TRUE)
library(Rcpp)
library(Barycenter)

## Visualize the 5 different handwritten 8's
image(eight[[1]])
image(eight[[2]])
image(eight[[3]])
image(eight[[4]])
image(eight[[5]])

## Compute the barycenter of the 5 handwritten 8's. This takes a while. You can compute a more approximate answer by changing the lambda parameter.
barycenter <- WaBarycenter(eight)
## less accurate with higher lambda value
##barycenter <- WaBarycenter(eight, lambda=10)

## visualize the computed barycenter.
image(barycenter)










# finding the average of the five eights (instead of the barycenter)
X <- eight[[1]]
for (i in 2:5) {
  X <- X + eight[[i]]
}
X <- (1/5)*X
image(X)











# computing the distance (EMD Calculation):

# Greenkhorn function  (less accurate)
n <- seq(0,1,length.out = dim(eight[[1]])[2])
# costm matrix = ground distance between points 
costm <- as.matrix(dist(expand.grid(n,rev(n)), diag=TRUE, upper=TRUE))
r <- matrix(eight[[1]],28*28,1)
c <- matrix(eight[[2]],1,28*28)
Greenkhorn(r, c, costm)$Distance


# Sinkhorn (more accurate)
Sinkhorn(matrix(eight[[1]],ncol=1), matrix(barycenter,ncol=1), costm)

dist1 <- Sinkhorn(matrix(eight[[1]],ncol=1), matrix(barycenter,ncol=1), costm)
str(dist1)
dist2 <- Sinkhorn(matrix(eight[[2]],ncol=1), matrix(barycenter,ncol=1), costm)
str(dist2)
dist3 <- Sinkhorn(matrix(eight[[3]],ncol=1), matrix(barycenter,ncol=1), costm)
str(dist3)
dist4 <- Sinkhorn(matrix(eight[[4]],ncol=1), matrix(barycenter,ncol=1), costm)
str(dist4)
dist5 <- Sinkhorn(matrix(eight[[5]],ncol=1), matrix(barycenter,ncol=1), costm)
str(dist5)


# K - means clustering: use the WH_kmeans() or WH_adaptive.kmeans() function in the HistDAWass package 

WH_adaptive.kmeans(x = WaBarycenter(eight), k = 1)





# Rong Code: Library help

library(HistDAWass)

# create lists of histogram distributions
img_eight<-vector("list",5)
for (i in 1:5) {
  img_eight[[i]] = data2hist(eight[[i]], type = "regular")
}

# combine separate lists into a matrix of histogram objects
mymat <- new("MatH", nrows=5, ncols=1, ListOfDist=img_eight, names.rows=c(1:5), names.cols="density")

# calculate clusters pre-specifying number of clusters (k)
WH_adaptive.kmeans(mymat, k=3)














## TURN THE MNIST DATASET INTO HISTOGRAM DATA AND USE WH_ADAPTIVE.KMEANS() TO CLUSTER IT


#install.packages("R.utils")

library(R.utils)
# mnist_data <- gunzip("C:\\Users\\Andrea\\Downloads\\train-images-idx3-ubyte.gz", remove=FALSE)
to.read = file("C:\\Users\\Andrea\\Downloads\\train-images-idx3-ubyte.gz", "rb")
readBin(to.read, integer(), n=1, endian="big")
readBin(to.read, integer(), n=1, endian="big")
readBin(to.read, integer(), n=1, endian="big")
readBin(to.read, integer(), n=1, endian="big")
readBin(to.read, integer(), n=1, endian="big")
readBin(to.read, integer(), n=1, endian="big")

to.read = file("C:\\Users\\Andrea\\Downloads\\train-images-idx3-ubyte.gz", "rb")
readBin(to.read, integer(), n=4, endian="big")
m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28)
image(m)
par(mfrow=c(5,5))
par(mar=c(0,0,0,0))
for(i in 1:25){m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28);image(m[,28:1])}




library(HistDAWass)

# create lists of histogram distributions
img_eight<-vector("list",5)
for (i in 1:5) {
  img_eight[[i]] = data2hist(eight[[i]], type = "regular")
}

# combine separate lists into a matrix of histogram objects
mymat <- new("MatH", nrows=5, ncols=1, ListOfDist=img_eight, names.rows=c(1:5), names.cols="density")

# calculate clusters pre-specifying number of clusters (k)
WH_adaptive.kmeans(mymat, k=3)










results <- WH_adaptive.kmeans(x = BLOOD, k = 2, rep = 10, 
                              simplify = TRUE, qua = 10, standardize = TRUE)























# GOT THIS TO WORK 

library(HistDAWass)

to.read = file("C:\\Users\\Andrea\\Downloads\\train-images.idx3-ubyte", 'rb')

# create lists of histogram distributions
img_mnist = vector("list",25)
for (i in 1:25) {
  m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28)
  img_mnist[[i]] = data2hist(m, type = "regular")
  image(m[,28:1])
}

to.read = file("C:\\Users\\Andrea\\Downloads\\train-images.idx3-ubyte", 'rb')
readBin(to.read, integer(), n=4, endian="big")
par(mfrow=c(5,5))
par(mar=c(0,0,0,0))
for(i in 1:25){m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28);image(m[,28:1])}

# combine separate lists into a matrix of histogram objects
mymat = new("MatH", nrows=25, ncols=1, ListOfDist=img_mnist, names.rows=c(1:25), names.cols="density")
WH_adaptive.kmeans(mymat, k=3)




library(tidyverse)
# time series covid data
coviddata <- read_csv("C:\\Users\\Andrea\\OneDrive\\Documents\\GitHub\\DST_ClusteringProject\\time_series_covid19_confirmed_global.csv")


