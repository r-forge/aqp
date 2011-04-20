library(aqp)

source("Class-SoilProfile.R")
source("Class-SoilProfileCollection.R")
source('SoilProfile-methods.R')
source('SoilProfileCollection-methods.R')
source('setters.R')

data(ca630)

# subset problematic pedon: NA in horizon depths
sp <- ca630$lab[ca630$lab$pedon_key == '07N0471', ]

# SoilProfile: error with missing hz
depths(sp) <- pedon_key ~ hzn_top + hzn_bot


# subset working pedon
sp <- ca630$lab[ca630$lab$pedon_key == '99P0314', ]
depths(sp) <- pedon_key ~ hzn_top + hzn_bot

# assign site data site SoilProfile (not implemented yet)
site(sp) <- subset(ca630$site, subset=pedon_key == '99P0314')


# copy, and clean bad depths
spc <- subset(ca630$lab, subset=!is.na(hzn_top) | !is.na(hzn_bot))

# SoilProfileCollection
depths(spc) <- pedon_key ~ hzn_top + hzn_bot

# check
# summary(spc)

# add site data... without specifying a site id, get a warning
site_id(spc)
site(spc) <- ca630$site
site_id(spc) <- 'pedon_key'

# must have column that can be used to link to profile data
site_id(spc) <- '@#$@xxx'

# BUG: this will clobber the site_id slot
site(spc) <- ca630$site

