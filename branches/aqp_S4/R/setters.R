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

function (object, value) 
{
    coord.numbers = NULL
    if (inherits(value, "formula")) {
        cc = model.frame(value, object)
        if (dim(cc)[2] == 2) {
            nm = as.character(as.list(value)[[2]])[2:3]
            coord.numbers = match(nm, names(object))
        }
        else if (dim(cc)[2] == 3) {
            nm = c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3], 
                as.character(as.list(value)[[2]])[3])
            coord.numbers = match(nm, names(object))
        }
    }
    else if (is.character(value)) {
        cc = object[, value]
        coord.numbers = match(value, names(object))
    }
    else if (is.null(dim(value)) && length(value) > 1) {
        if (any(value != as.integer(value) || any(value < 1))) 
            stop("coordinate columns should be positive integers")
        cc = object[, value]
        coord.numbers = value
    }
    else cc = coordinates(value)
    if (any(is.na(cc))) 
        stop("coordinates are not allowed to contain missing values")
    if (!is.null(coord.numbers)) {
        object = object[, -coord.numbers, drop = FALSE]
        stripped = coord.numbers
        if (ncol(object) == 0) 
            return(SpatialPoints(cc))
    }
    else stripped = numeric(0)
    SpatialPointsDataFrame(coords = cc, data = object, coords.nrs = stripped, 
        match.ID = FALSE)
}
