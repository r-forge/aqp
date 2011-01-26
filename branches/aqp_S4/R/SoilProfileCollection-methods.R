#' SoilProfileCollection objects are storing...
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param depth_units the unit in which horizon depths are expressed
#'
"SoilProfileCollection" <- function(profiles=list(SoilProfile()), site=data.frame(), ids=rep(as.character(NA), length.out=length(profiles))){
  # if all the ids are undefined, the are given an arbitrary id
  if (all(is.na(ids)))
    ids <- as.character(1:length(ids))

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
  }
)

## accessors

if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

# retrieves the site data frame
setMethod("site", "SoilProfileCollection",
  function(object) 
    object@site
)

if (!isGeneric("profiles"))
  setGeneric("profiles", function(object, ...)
    standardGeneric("profiles"))

# retrieves the SoilProfile objects - all or one in particular, specified by its index or its id
setMethod("profiles", "SoilProfileCollection",
  function(object, id=as.numeric(NA)) {
    if (is.na(id))
      id <- 1:length(object@profiles)
    else
      if (!is.numeric(id))
	id <- which(ids(object) %in% id)
    res <- object@profiles[id]
    if (length(res) == 1)
      res <- res[[1]]
    res
  }
)

if (!isGeneric("ids"))
  setGeneric("ids", function(object, ...)
    standardGeneric("ids"))

# retrieves the ids
setMethod("ids", "SoilProfileCollection",
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