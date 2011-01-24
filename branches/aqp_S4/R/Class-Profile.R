setClass(
  Class="Profile", 
  representation=representation(
    depths='matrix',
    horizons='data.frame',
    id='character', # change it to profile_id ??
    depth_units='character'
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
    horizons=data.frame(),
    id=as.character(NA),
    depth_units=as.character(NA)
  ),
  validity=function(object) {
    # If there is horizon data available
    if (length(object@horizons) > 0 ) {
      # number of horizon and number of horizon data must match
      if (nrow(object@horizons) != nrow(object@depths))
        stop("number of horizons and number of samples along the profile do not match")
    }
     # Number of user_id must equals one (one per profile)
    if (length(object@id) != 1)
      stop("profile id must be unique")
    return(TRUE)     
  }
)