library(sp)
library(maptools)
library(rgdal)
library(plyr)

## NSSL
# site data
s <- read.csv('CA630-site.csv', as.is=TRUE)
# lab data
lab <- read.csv('CA630-CEC_and_Bases.csv', as.is=TRUE)
# pedon data
p <- read.csv('CA630-pedon.csv', as.is=TRUE)

##############################################################################
############################## site data #####################################
##############################################################################

# identify missing coordinates
s.vars <- c('longitude_degrees', 'longitude_minutes', 'longitude_seconds', 'latitude_degrees','latitude_minutes','latitude_seconds')
s.no.missing.idx <- which(complete.cases(s[, s.vars]))

# add cols to original DF
s$lon <- NA
s$lat <- NA
s$proj4 <- NA

# fix coordinates -- DMS to DD, then add to DF
s$lon <- with(s, -(longitude_degrees + (longitude_minutes/60) + (longitude_seconds/60/60)))
s$lat <- with(s, latitude_degrees + (latitude_minutes/60) + (latitude_seconds/60/60))

# convert to single datum
s$datum <- with(s, ifelse(horizontal_datum_name == '', 'WGS84', horizontal_datum_name))

s$proj4[s.no.missing.idx] <- paste('+proj=longlat +datum=', s$datum[s.no.missing.idx], sep='')

# iterate over valid coordinates, and convert to WGS84
s.final.coords <- ddply(s[s.no.missing.idx, ], .(user_site_id), 
.progress='text', .fun=function(i) {
  coordinates(i) <- ~ lon + lat
	proj4string(i) <- CRS(i$proj4)
	i.t <- spTransform(i, CRS('+proj=longlat +datum=WGS84'))
	dimnames(i.t@coords)[[2]] <- c('x','y')
	as.matrix(coordinates(i.t))
	})


# merge with original data
s.final <- join(s, s.final.coords, by='user_site_id')

# keep only some columns
s.final.vars <- c('user_site_id','mlra','county','ssa','x','y')
# subset columns, and save to temp CSV for now
ca630.site <- s.final[, s.final.vars]


##############################################################################
############################## pedon data ####################################
##############################################################################

# keep only some columns
# combine with site data
ca630.pedon <- p[, 1:9]
ca630.site <- join(ca630.site, ca630.pedon, by='user_site_id')


##############################################################################
############################## lab data #####################################
##############################################################################

ca630.lab <- lab



##############################################################################
############################## combine and save ##############################
##############################################################################
ca630 <- list(site=ca630.site, lab=ca630.lab)
save(ca630, file='ca630.rda')