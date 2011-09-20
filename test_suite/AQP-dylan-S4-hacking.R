library(sp)
library(aqp)

source('SoilProfileCollection-methods.R')
source('Class-SoilProfileCollection.R')
source('setters.R')

data(sp1)
sp1$x <- rnorm(nrow(sp1))
sp1$y <- rnorm(nrow(sp1))

depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group


