## depths<- setter method - to create AQP objects
##
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
	  res <- SoilProfile(res, horizons=horizon_data)	  
      }
      else { # otherwise, we have a collection -> SoilProfileCollection
	  # nm contains names for profiles_ids, top, bottom
	  SP_list <- dlply(.data=object, .variables=nm[1],
		  .fun=function(x) {
		    # get current user_id
		    profile_id_i <- unique(as.character(x[, idx[1]]))
		    # extract depths
		    depths <- as.matrix(x[, idx[2:3]])
		    # make a copy of the horizon data, with id, top, and bottom removed
		    horizon_data <- x[, -idx]
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

## site<- setter method - to initialize site data
## in SoilProfileDataFrame and SoilProfileCollection objects
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value) 
    standardGeneric('site<-'))

setReplaceMethod("site", "Profile",
  function(object, value) {
    # cant do that on a Profile object as it does not have any data at all (excpet depths)
    if (inherits(value, "formula") & (class(object) != "Profile")) {
      mf <- model.frame(value, horizons(object))
      idx <- match(names(mf), names(horizons(object)))

      # assemble site_data, this is a 1-row data.frame
      # since these data are repeated for each horizon, just keep the first
      
      # when there is only one attribute for site data we need to use a different approach
      if(ncol(mf) < 2) {
	site_data <- data.frame(X1=mf[1,])
	names(site_data) <- names(mf)
      }
      # otherwise we just take the first row
      else {
	# checking unicity of the attributes along the profile
	id_error <- which(sapply(mf, function(x){length(unique(x))}) > 1)
	if (length(id_error) == 0)
	  site_data <- mf[1, ]
	else
	  stop('site values are not site-specific')
      }
      
      # remove the named site data from horizon_data
      object@horizons <- horizons(object)[, -idx]
      # update or create the site data
      if (inherits(object, "SoilProfileDataFrame"))
	object@site <- site_data # assign to object's slot
      else # creation of a SoilProfileDataFrame object
	object <- SoilProfileDataFrame(object, site=site_data)
    }

    else
      stop('not implemented yet')
  }
)

# function to generate a random char string
.createCharHash <- function(n, prefix=""){
  require(stringr)
  hash <- letters[round(26*runif(n))]
  str_c(c(prefix, hash), collapse = "")
}

setReplaceMethod("site", "SoilProfileCollection",
  function(object, value) {
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object))
      names_attr <- names(mf)
      idx <- match(names(mf), names(horizons(object)))
      # Adding a profile id column
      mf <- data.frame(mf, 
	unlist(
	  llply(profile_id(object), 
	    function(x){
	      rep(x, length.out=length(profiles(object, x)))
	    })
	, use.names=F))
      # for identification of the profiles, a hash is used 
      # so we don't mess up with the *real* attributes names.
      # if theres a confusion you really have strange attr names
      tmp_id <- .createCharHash(n=5, prefix='TMP') 
      names(mf) <- c(names_attr, tmp_id)
      # assemble site_data, this is a 1-row data.frame
      # since these data are repeated for each horizon, just keep the first
      
      # when there is only one attribute for site data we need to use a different approach
      site_data <- ddply(mf, tmp_id, 
	  .fun=function(x) {
	    df <- subset(x, select=names_attr)
	    colwise(unique)(df)
	  })
      tmp_id_col <- which(names(site_data) == tmp_id)
      site_data <- site_data[,-tmp_id_col]
      
      if (!inherits(site_data, 'data.frame')) {
	site_data <- as.data.frame(site_data)
	names(site_data) <- names_attr
      }
      # update/create the site data slot
      object@site <- site_data
      # remove the named site data from horizon_data IN EACH PROFILE
#       l_ply(.data=object, .fun=function(x){
# 	idx <- match(names_attr, names(horizons(x)))
# 	horizons(x) <- horizons(x)[, -idx]
#       })
    }
    else 
      stop('not implemented yet')
    object
  }
)

## horizons<- setter method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) 
    standardGeneric('horizons<-'))

setReplaceMethod("horizons", "Profile",
  function(object, value) {
    # testing the class of the horizon data to add to the object
    if (!inherits(value, "data.frame"))
      stop("value must be a data.frame")
    # testing the number of rows of the horizon data
    if (nrow(value) != length(object))
      stop("inconsistent number of rows")
    # testing if a horizons slot is available
    if (class(object) == "Profile")
      object <- SoilProfileDataFrame(object, horizons=value)
    else 
      object@horizons <- value
    object
  }
)