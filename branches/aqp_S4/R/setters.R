## initialize SP/SPC objects from a model.frame
##
.initSPCfromMF <- function(data, mf){
  # get the names and column indices of the id, top, bottom
  # so that we can remove them latter
  nm <- names(mf)
  idx <- match(nm, names(data))
  
  # generate profile(s) ID(s) and depths
  profiles_id <- unique(as.character(mf[, 1]))
  names(profiles_id) <- rep(nm[1], length(profiles_id))

  # if there is only 1 ID we are generating a SoilProfile object
  if(length(profiles_id) == 1) {
    # extract depths
    depths <- as.matrix(data[, idx[2:3]])      
    # make a copy of the horizon data, with id, top, and bottom removed
    horizon_data <- data[, -idx]
    
    # if horizon data is a vector we need to convert it
    # as a data.frame so it passes the validity tests
    if ((!is.data.frame(horizon_data)) & (length(horizon_data) > 0)) {
      horizon_data <- as.data.frame(horizon_data)
      names(horizon_data) <- names(data)[4]
    }
 
    # we create a SoilProfile object
    res <- SoilProfile(depths=depths, id=profiles_id, horizons=horizon_data)	  
  }
  else { # otherwise, we have a collection -> SoilProfileCollection
    SP_list <- dlply(.data=data, .variables=nm[1],
      .fun=function(x) {
	# get current user_id
	profile_id_i <- unique(as.character(x[, idx[1]]))
	names(profile_id_i) <- names(x)[idx[1]]
	# extract depths
	depths <- as.matrix(x[, idx[2:3]])
	# make a copy of the horizon data, with id, top, and bottom removed
	horizon_data <- x[, -idx]
	# if horizon data is a vector we need to convert it
	# as a data.frame so it passes the validity tests
	if ((!is.data.frame(horizon_data)) & (length(horizon_data) > 0)) {
	  horizon_data <- as.data.frame(horizon_data)
	  names(horizon_data) <- names(x)[4]
	} 
	# assemble object and return
	SoilProfile(depths=depths, horizons=horizon_data, id=profile_id_i)
      }
    )
    # assemble and return the SoilProfileCollection object
    res <- SoilProfileCollection(profiles=SP_list)
  }
  res
}

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
      res <- .initSPCfromMF(data=object, mf=mf)
    }	
    else {
      if (inherits(value, "character")) { # initialization by colnames
	mf <- object[,value]
	res <- .initSPCfromMF(data=object, mf=mf)
      }
      else
	stop('invalid initialization for SoilProfile object')
    }
    res
  }
)

## site<- setter method - to initialize site data
## in SoilProfileCollection objects
##
if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, value, ...) 
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

# function to generate a list of profiles and a site data
# frame - ready to be used to initialize/update a SPC
.createSiteFromHorizon <- function(object, mf){
  names_attr <- names(mf)
  idx <- match(names(mf), names(horizons(object)))
  
  # when there is only one attribute for site data we need to use a different approach
  site_data <- ddply(mf, idname(object), 
      .fun=function(x) {
	df <- subset(x, select=names_attr)
	colwise(unique)(df)
      })
  tmp_id_col <- which(names(site_data) == idname(object))
  site_data <- site_data[,-tmp_id_col]
  
  if (!inherits(site_data, 'data.frame')) {
    site_data <- as.data.frame(site_data)
    names(site_data) <- names_attr[-tmp_id_col]
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
  function(object, value, site_id=NA) {
    ids <- unlist(llply(profiles(object), 
      .fun=function(x)rep(profile_id(x), length(x))
      ), use.names=FALSE)
  # creation of site data from horizon data
    if (inherits(value, "formula")) {
      mf <- model.frame(value, horizons(object))
      nm <- names(mf)
      mf <- data.frame(ids, mf)
      names(mf) <- c(idname(object), nm)
      res <- .createSiteFromHorizon(object, mf)
      object <- SoilProfileCollection(profiles=res$profiles_list, site=res$site_data)
    }
    else {
      if (inherits(value, "character")) {
	i <- which(names(horizons(object)) %in% value)
	mf <- horizons(object)[, i]
	if (!is.data.frame(mf)) {
	  mf <- data.frame(mf)
	  names(mf) <- names(horizons(object))[i]
	}
	nm <- names(mf)
	mf <- data.frame(ids, mf)
	names(mf) <- c(idname(object), nm)
	res <- .createSiteFromHorizon(object, mf)
	object <- SoilProfileCollection(profiles=res$profiles_list, site=res$site_data)
      }
  # creation of site data from external data
      else {
	if (inherits(value, "data.frame")) {
	# if this is a data.frame we are actually adding data
	object <- SoilProfileCollection(profiles=as.list(profiles(object)), site=value)
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
    object@horizons <- value
    object
  }
)

setReplaceMethod("horizons", "SoilProfileCollection",
  function(object, value) {
    # testing the class of the horizon data to add to the object
    if (!inherits(value, "data.frame"))
      stop("value must be a data.frame")
    # testing the number of rows of the horizon data
    if (nrow(value) != nrow(horizons(object)))
      stop("inconsistent number of rows")
    profiles_list <- llply(profiles(object), function(x) {
      i <- which(horizons(object, keep.id=TRUE)[[idname(object)]] == profile_id(x))
      horizons(x) <- value[i,]
      x
    })
    object <- SoilProfileCollection(profiles=profiles_list, site=site(object))
    object
  }
)

## profiles<- setter method

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
    
    # replace collection-level @ids slot
    # note that the 'names' attribute is lost...
    object@ids <- value
    
    # replace each individual SoilProfile's id'
    p <- profiles(object)
    
    # init empty list to hold SoilProfiles with new IDs
    p.list <- vector(length=length(p), mode='list')
    
    # iterate over old list, assigning new IDs, saving to p.list
    for(i in seq_along(value))	{
	  p.i <- p[[i]]
	  
	  # important: the new id must have a names attribute of 'id' !!
	  v.i <- value[i]
	  attr(v.i, which='names') <- 'id'
	  
	  # update this profile ID
	  profile_id(p.i) <- v.i
	  # append to our new list of SoilProfile objects
	  p.list[[i]] <- p.i
	}
    
    # replace original profiles with new, updated list of SoilProfile objects
    profiles(object) <- p.list
    
    # done
    object
  })
  