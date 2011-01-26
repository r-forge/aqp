#' SoilProfileCollection objects are storing...
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param depth_units the unit in which horizon depths are expressed
#'
"SoilProfileCollection" <- function(profiles=list(SoilProfile()), site=data.frame(), ids=as.character(NA)){
  # default is that the ids are the id of the SoilProfile objects
  if (is.na(ids))
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
  if (!all(is.na(x[["site"]]))) {
    cat("\nSampling sites attributes:\n")
    print(x[["site"]])
  }

  invisible(x)
}

setMethod("print", "summary.SoilProfileCollection", print.summary.SoilProfileCollection)

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

# horizons data accessor

if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object ,id=as.numeric(NA)){
    # if no profile id, the data for every profile is returned
    if (all(is.na(id)))
      res <- ldply(profiles(object), horizons)
    else {
      if (!is.numeric(id))
	id <- which(profile_id(object) %in% id)
      res <- ldply(profiles(object)[id], horizons)
    }
    res
  }
)

if (!isGeneric("profiles"))
  setGeneric("profiles", function(object, ...)
    standardGeneric("profiles"))

# retrieves the SoilProfile objects - all or one in particular, specified by its index or its id
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
  function(object)
    unique(sapply(profiles(object), depths_units))
)

if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

# retrieves the ids
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