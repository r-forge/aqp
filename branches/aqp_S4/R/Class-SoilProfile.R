setClass(
  Class='SoilProfile', 
  representation=representation(
    "Profile",
    site='data.frame'
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
    horizons=data.frame(),
    id=as.character(NA),
    depth_units=as.character(NA),
    site=data.frame()
  ),
  validity=function(object) {
     # By definition site_data has one value per profile
    if (nrow(object@site) > 1)
      stop("site data has to have one per value property.")
    return(TRUE)     
  }
)