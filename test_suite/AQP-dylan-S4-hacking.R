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



## test a more realistic data set
data(ca630)

# combine into single DF
ca <- join(ca630$lab, ca630$site)

# init SPC
depths(ca) <- pedon_id ~ top + bottom

# extract site data
site(ca) <- ~ mlra + ssa + lon + lat + user_pedon_id + control_sec_top + control_sec_bottom + series + soiltax

# extract spatial data as SpatialPoints
coordinates(ca) <- ~ lon + lat



