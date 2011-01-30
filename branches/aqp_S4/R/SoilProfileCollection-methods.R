#' SoilProfileCollection objects are storing...
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param depth_units the unit in which horizon depths are expressed
#'
"SoilProfileCollection" <- function(profiles=list(SoilProfile()), site=data.frame(), ids=as.character(NA)){
  # default is that the ids are the id of the SoilProfile objects
  if (all(is.na(ids)))
    ids <- sapply(profiles, profile_id)
  names(profiles) <- ids
  # creation of the object (includes a validity check)
  new("SoilProfileCollection", profiles=profiles, site=site, ids=ids)
}

## show

setMethod(
  f='show',
  signature='SoilProfileCollection',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("Number of profiles: ", length(object), "\n", sep="")
    if (length(object) > 0) {
      cat("Depth range: ", min(object), "-", max(object), " ", depths_units(object), "\n", sep="")
      cat("\nAvailable profiles:\n")
      print(profiles(object))
    }
    if (length(site(object)) > 0) {
      cat("\nSampling sites attributes:\n")
      print(site(object))
    }
  }
)

## summary

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

summary.SoilProfileCollection <- function (object, ...){
    obj <- list()
    obj[["class"]] = class(object)
    obj[["n_profiles"]] <- length(object)
    if (length(object) > 0)
      obj[["depth_range"]] <- c(min(object), max(object))
    else
      obj[["depth_range"]] <- NA
    if (length(profiles(object)) > 0) {
      # on how many profiles do we have horizon data?
      is.SoilProfile <- laply(.getProfilesAsList(object), function(x){'horizons' %in% slotNames(x)})
      obj[["n_horizons_data"]] <- length(which(is.SoilProfile))
      if (obj[["n_horizons_data"]] > 0) { # if data is availbale we store its summary
	obj[['horizons']] <- summary(horizons(object))
      }
    }
    else # no profile in the collection
      obj[["n_horizons_data"]] <- NA
    if (length(site(object)) > 0)
      obj[["site"]] <- summary(site(object))
    else
      obj[["site"]] <- NA
    obj[["units"]] <- depths_units(object)
    class(obj) <- "summary.SoilProfileCollection"
    obj
}

setMethod("summary", "summary.SoilProfileCollection", summary.SoilProfileCollection)

print.summary.SoilProfileCollection = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Number of profiles: ", x[["n_profiles"]], "\n", sep="")
  if (x[["n_profiles"]] > 0)
    cat("Depth range: ", x[["depth_range"]][1], "-", x[["depth_range"]][2], " ", x[["units"]], "\n", sep="")
  if (!all(is.na(x[["n_horizons_data"]]))) {
    if (x[["n_horizons_data"]] > 0) {
      if (x[["n_horizons_data"]] < x[["n_profiles"]]) {
	cat("\nHorizon data available on ", x[["n_horizons_data"]], " out of ", x[["n_profiles"]], " profiles in the collection:\n")
      }
      else {
	cat("\nHorizon data available on every profile in the collection:\n")
      }
      print(x[["horizons"]])
    }
    else
      cat("No horizon data available in the collection.\n")
  }
  if (!all(is.na(x[["site"]]))) {
    cat("\nSampling sites attributes:\n")
    print(x[["site"]])
  }
  else 
    cat("No sampling sites data available in the collection.\n")

  invisible(x)
}

setMethod("print", "summary.SoilProfileCollection", print.summary.SoilProfileCollection)

## internal functions

# to be sure to be returned a list of profiles
# even when n profiles == 1
.getProfilesAsList <- function(object){
  if (!is.list(profiles(object)))
    profiles_list <- list(profiles(object))
  else 
    profiles_list <- profiles(object)
  profiles_list
}

## coerce

as.data.frame.SoilProfileCollection = function(object, ...) {
  id <- unlist(llply(profiles(object), function(x){rep(profile_id(x), length(x))}), use.names=FALSE)
  if (length(site(object)) == 0)
    res <- data.frame(id=id, ldply(depths(object), rbind.data.frame), horizons(object))
  else {
    n_horizons <- llply(profiles(object), length)
    site <- sapply(site(object), function(x){rep(x, n_horizons)})
    res <- data.frame(id=id, site, ldply(depths(object), rbind.data.frame), horizons(object))
  }
  res
}

setAs("SoilProfileCollection", "data.frame", function(from)
	as.data.frame.SoilProfileCollection(from))

## accessors

if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) {
    res <- object@site
    if (length(res) > 0)
      rownames(res) <- profile_id(object)
    res
  }
)

# retrieves list of profiles horizons depths 
if (!isGeneric("depths"))
  setGeneric("depths", function(object, ...)
    standardGeneric("depths"))

setMethod("depths", "SoilProfileCollection",
  function(object, na.rm=TRUE){
    llply(.getProfilesAsList(object), depths, na.rm=na.rm)
  }
)

# returns a data.frame aggregating horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object ,id=as.numeric(NA)){
    if (all(is.na(id))) { # if no profile id, the data for every profile is returnedt)
      res <- ldply(.getProfilesAsList(object), horizons)
    }
    else {
      if (!is.numeric(id))
	id <- which(profile_id(object) %in% id)
      res <- ldply(profiles(object)[id], horizons)
    }
    res
  }
)

# retrieves the SoilProfile objects - all or one in particular, 
# if specified by its index or its id
if (!isGeneric("profiles"))
  setGeneric("profiles", function(object, ...)
    standardGeneric("profiles"))

setMethod("profiles", "SoilProfileCollection",
  function(object, id=as.numeric(NA)) {
    if (all(is.na(id)))
      id <- 1:length(object@profiles)
    else
      if (!is.numeric(id))
	id <- which(profile_id(object) %in% id)
    res <- object@profiles[id]
    if (length(res) == 1)
      res <- res[[1]]
    res
  }
)

if (!isGeneric("depths_units"))
  setGeneric("depths_units", function(object, ...)
    standardGeneric("depths_units"))

setMethod("depths_units", "SoilProfileCollection",
  function(object) {
    unique(laply(.getProfilesAsList(object), depths_units))
  }
)

# retrieves the ids
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfileCollection",
  function(object) 
    object@ids
)

## overloads

# overload min() to give us the min depth within a collection
setMethod(f='min', signature='SoilProfileCollection',
definition=function(x)
    min(sapply(x@profiles, min))
)

# overload max() to give us the max depth within a collection
setMethod(f='max', signature='SoilProfileCollection',
definition=function(x)
    max(sapply(x@profiles, max))
)

# overload length() to give us the number of profiles in the collection
setMethod(f='length', signature='SoilProfileCollection',
  definition=function(x){
    length(profiles(x))
  }
)