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

	# we create a SoilProfile object
	res <- SoilProfile(depths=depths, id=profiles_id, horizons=horizon_data)	  
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
## in SoilProfileCollection objects
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value) 
    standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfile",
  function(object, value) {
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object))
      idx <- match(names(mf), names(horizons(object)))
      
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
      horizons(object) <- horizons(object)[, -idx]

      # if site data is already present in the object, we don't want to erase it
      if (length(site(object)) > 0)
	site_data <- data.frame(site(object), site_data)

      object <- SoilProfileCollection(profiles=list(object), site=site_data)
    }
    else
      stop('not implemented yet')
    object
  }
)

# function to generate a random char string
.createCharHash <- function(n, prefix=""){
  require(stringr)
  hash <- letters[round(26*runif(n))]
  str_c(c(prefix, hash), collapse = "")
}

.createSiteFromHorizon <- function(object, mf){
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

  # if site data is already present in the object, we don't want to erase it
  if (length(site(object)) > 0)
    site_data <- data.frame(site(object), site_data)

  # remove the named site data from horizon_data IN EACH PROFILE
  # note that we are replacing the list of SoilProfile objects (in place) with modified versions
  profiles_list <- lapply(profiles(object), function(i, v.names=names_attr) {
    h <- horizons(i)
    select_cols <- setdiff(names(h), v.names)
    horizons(i) <- subset(h, select=select_cols)
    return(i) 
  })
  list(profiles_list=profiles_list, site_data=site_data)
}

setReplaceMethod("site", "SoilProfileCollection",
  function(object, value) {

  # creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object))
      res <- .createSiteFromHorizon(object, mf)
      object <- SoilProfileCollection(profiles=res$profiles_list, site=res$site_data)
    }
    else {
      if (inherits(value, "character")) {
	i <- which(names(horizons(object)) %in% value)
	mf <- horizons(object)[, i]
	res <- .createSiteFromHorizon(object, mf)
	object <- SoilProfileCollection(profiles=res$profiles_list, site=res$site_data)
      }
  # creation of site data from external data
      else {
	if (inherits(value, "data.frame")) {
	# if this is a data.frame we are actually adding data
	object <- SoilProfileCollection(profiles=profiles(object), site=value)
	}
	else stop('not implemented yet')
      }
    }
    object
  }
)

## horizons<- setter method
##
if (!isGeneric('horizons<-'))
  setGeneric('horizons<-', function(object, value) 
    standardGeneric('horizons<-'))

setReplaceMethod("horizons", "SoilProfile",
  function(object, value) {
    # testing the class of the horizon data to add to the object
    if (!inherits(value, "data.frame"))
      stop("value must be a data.frame")
    # testing the number of rows of the horizon data
    if (nrow(value) != length(object))
      stop("inconsistent number of rows")
    # if horizon data is already present
#     if (length(horizons(object)) > 0)
#       value <- data.frame(horizons(object), value)
    object@horizons <- value
    object
  }
)

## profiles<- setter method
##
if (!isGeneric('profiles<-'))
  setGeneric('profiles<-', function(object, value) 
    standardGeneric('profiles<-'))

setReplaceMethod("profiles", "SoilProfileCollection",
  function(object, value) {
    # this setter aims at replacing one or several profiles in the collection
    # without using the @ slot accessor (but using the validity checks). 
    # 

    # Only one profile is given
    if (inherits(value, "SoilProfile")) {
      object <- SoilProfileCollection(profiles=list(value), site=site(object))
    }
    else
      # More than one profile - but it has to ba a list of SoilProfile!
      if (inherits(value, "list") & (all(laply(value, inherits, "SoilProfile"))))
	object <- SoilProfileCollection(profiles=value, site=site(object))
      else stop("wrong profile re-initialization")
    object
  }
)

## profile_id<- setter

# this setter aims at replacing the ID of a SoilProfile
# 
if (!isGeneric('profile_id<-'))
  setGeneric('profile_id<-', function(object, value) 
    standardGeneric('profile_id<-'))

setReplaceMethod("profile_id", "SoilProfile",
  function(object, value) {
    if (length(value) != 1)
      stop("length of the new ID does not match the length of the object")
    if (!is.character(value))
      value <- as.character(value)
    object@id <- value
    object
  })

setReplaceMethod("profile_id", "SoilProfileCollection",
  function(object, value) {
    # check length of the object
    if (length(value) != length(object))
      stop("length of the new IDs does not match the length of the object")
    if (!is.character(value))
      value <- as.character(value)
    # check unicity of IDs in value
    if (length(unique(value)) < length(value)) {
      value <- make.unique(value)
      warning("You entered duplicated IDs. This has been corrected but you may want to check the new IDs.")
    }
    object@ids <- value
    object
  })