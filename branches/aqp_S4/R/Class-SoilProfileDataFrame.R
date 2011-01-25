setClass(
  Class='SoilProfileDataFrame', 
  representation=representation(
    "SoilProfile", # a SoilProfile object gives depths, id, units, and horizon data
    site='data.frame' # the data related to the site
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
    id=as.character(NA),
    depth_units=as.character(NA),
    horizons=data.frame(),
    site=data.frame()
  ),
  validity=function(object) {
     # By definition site_data has one value per profile
    if (nrow(object@site) > 1)
      stop("site data has to have one per value property.")

    return(TRUE)     
  }
)