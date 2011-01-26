if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) 
    standardGeneric('depths<-'))

setReplaceMethod("depths", "data.frame",
  function(object, value) {
    if (inherits(value, "formula")) {
      # extract components of formula
      # 1. user id, 2. top, 3. bottom
      mf <- model.frame(value, object)
      
      # get the names and column indices of the id, top, bottom
      # so that we can remove them latter
      nm <- names(mf)
      idx <- match(nm, names(object))
      
      # generate profile(s) ID(s) and depths
      profiles_id <- unique(as.character(mf[, 1]))
      
      # if there is only 1 ID we are generating a SoilProfile object
      if(length(profiles_id) == 1) {
	# extract depths
	depths <- as.matrix(object[, idx[2:3]])      
	# make a copy of the horizon data, with id, top, and bottom removed
	horizon_data <- object[, -idx]

	res <- Profile(depths=depths, id=profiles_id) # assemble as a Profile object
	if (nrow(horizon_data) > 0) # if theres data left, we create a SoilProfile object
	  res <- SoilProfile(depths, horizons=horizon_data)	  
      }

      # otherwise, we have a collection -> SoilProfileCollection
      else {
# MUST BE HANDLED DURING THE INSTALL OF THE PACKAGE
# 	if(!require(plyr)) # check for dependencies
# 	  stop('Please install the "plyr" package.')
	  
	  # nm contains names for profiles_ids, top, bottom
	  SP_list <- dlply(.data=object, .variables=nm[1], .progress='text',
		  .fun=function(profile_i) {
		    # get current user_id
		    profile_id_i <- unique(as.character(profile_i[, idx[1]]))
		    # extract depths
		    depths <- as.matrix(profile_i[, idx[2:3]])
		    # make a copy of the horizon data, with id, top, and bottom removed
		    horizon_data <- profile_i[, -idx]
		    # assemble object and return
		    SoilProfile(depths=depths, horizons=horizon_data, id=profile_id_i)
		  }
	  )
	  
	  # assemble and return the SoilProfileCollection object
	  res <- SoilProfileCollection(profiles=SP_list)
      }	
    }	
    else
      stop('invalid initialization for SoilProfile object')

    res
  }
)