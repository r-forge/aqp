library(plyr)
library(aqp)

x <- read.csv('OSACA-example_soil_data.csv')

# assume horizons are measured from 0
x.new <- ddply(x, .(Soil), .fun=function(i) {data.frame(i, top=c(0, i$Depth.D.[-length(i$Depth.D.)]), bottom=i$Depth.D.)})

# fix depths
x.new$top <- as.integer(x.new$top)
x.new$bottom <- as.integer(x.new$bottom)

# temp id
x.new$id <- as.numeric(x.new$Soil)

# give it a try
d <- profile_compare(x.new, vars=c('Sand.D.','MG.D.','EC.D.'), replace_na=TRUE, max_d=100, k=0, add_soil_flag=TRUE)

# figure out column names / meanings\
# R,G,B appear to be color data
x.new$soil_color <- rgb(x.new$R25.D., g=x.new$G25.D., b=x.new$B25.D.)
plot(Depth.D. ~ id, data=x.new, col=x.new$soil_color, pch=15, ylim=c(400, 0))

xx <- x.new[1:200, ]
depths(xx) <- id ~ top + bottom
profile_plot(xx, name='hor', max.depth=250)

# save to .Rdata
# document
# package