setClass(
  Class='SoilProfile', 
  representation=representation(
    "Profile", # a Profile object gives depths, id, units
    horizons='data.frame' # the data for each horizon
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
    id=as.character(NA),
    depth_units=as.character(NA),
    horizons=data.frame()
  ),
  validity=function(object) {
    # If there is horizon data available
    if (length(object@horizons) > 0 ) {
      # number of horizon and number of horizon data must match
      if (nrow(object@horizons) != nrow(object@depths))
        stop("number of horizons and number of samples along the profile do not match")
    }

    return(TRUE)     
  }
)