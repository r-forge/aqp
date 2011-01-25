setClass(
  Class='SoilProfile', 
  representation=representation(
    "Profile", # a Profile object gives depths, id, units
    horizons='data.frame', # the data for each horizon
#     site='data.frame' # the data related to the site
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
    id=as.character(NA),
    depth_units=as.character(NA),
    horizons=data.frame(),
#     site=data.frame()
  ),
  validity=function(object) {
    # If there is horizon data available
    if (length(object@horizons) > 0 ) {
      # number of horizon and number of horizon data must match
      if (nrow(object@horizons) != nrow(object@depths))
        stop("number of horizons and number of samples along the profile do not match")
    }
#      # By definition site_data has one value per profile
#     if (nrow(object@site) > 1)
#       stop("site data has to have one per value property.")

    return(TRUE)     
  }
)