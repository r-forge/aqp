library(aqp)

source('SoilProfileCollection-methods.R')
source('Class-SoilProfileCollection.R')
source('setters.R')

data(sp1)
sp1$x <- rnorm(nrow(sp1))
sp1$y <- rnorm(nrow(sp1))
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

depths(sp1) <- id ~ top + bottom
units(sp1) <- 'cm'

site(sp1) <- ~ group

profile_plot(sp1)
