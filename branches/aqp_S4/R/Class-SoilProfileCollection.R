setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    profiles='list', # list of SoilProfiles (ie profiles depths + horizon data)
    site="data.frame", # data about the sampling sites
    ids="character", # list of the profiles ids
    site_id="character" # name of column in site data that contains an id matching horizon data
  ),
  prototype=prototype(
    profiles=list(new("SoilProfile")),
    site=data.frame(),
    ids=as.character(NA),
    site_id=as.character(NA)
  ),
  validity=function(object) {
    # number of ids and number of profiles must match
    if (length(which(!is.na(object@ids))) != length(object@profiles))
      stop("number of ids and number of profiles must match")
    # if there is some site data
    if (length(object@site) > 0) {
      # number of ids and number of sites must match  
      if (length(which(!is.na(object@ids))) != nrow(object@site))
		 stop("number of ids and number of sites must match")
	}
    
    return(TRUE)
  }
)