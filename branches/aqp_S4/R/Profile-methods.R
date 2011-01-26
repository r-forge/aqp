## initializer

#' Initialises a new Profile object.
#' 
#' Profile objects are storing very basic metadata about a soil profile: 
#' depths, id, and the units in which depths are expressed.
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param depth_units the unit in which horizon depths are expressed
#'
"Profile" <- function(depths=matrix(ncol=2), id=as.character(NA), depth_units="cm"){
  # if the id is not given, it is initilialized at character(1).
  if (is.na(id))
    id <- as.character(1)
  # the id *must* be of class character (this is in the class definition)
  if (!is.character(id))
    id <- as.character(id)
  # giving the depths matrix standardized headers
  colnames(depths) <- c("top", "bottom")
  # creation of the object (includes a validity check)
  new("Profile", depths=depths, id=id, depth_units=depth_units)
}

## show

setMethod(
  f='show',
  signature='Profile',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("ID: ", profile_id(object), "\n", sep="")
    cat("Number of horizons: ", length(object), "\n", sep="")
    if (length(object) > 0) {
      cat("Available depths:\n")
      depth_classes <- aaply(depths(object), 1, function(x){paste(x[1], '-', x[2], ' ', depths_units(object), sep='')}, .expand=FALSE)
      sapply(depth_classes, function(x){cat(x, "\n", sep="")})
    }
    # If there are horizon data in the object (SoilProfile and SoilProfileDataFrame classes)
    if ("horizons" %in% slotNames(object)) {
      cat("\nHorizons attributes:\n")
      print(horizons(object))
    }
    # If there are site data in the object (SoilProfileDataFrame class)
    if ("site" %in% slotNames(object)) {
      cat("\nSampling site attributes:\n")
      print(site(object))
    }
  }
)

## summary

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

summary.Profile <- function (object, ...){
    obj <- list()
    obj[["class"]] = class(object)
    if (length(object) > 0)
      obj[["depths"]] <- depths(object)
    else
      obj[["depths"]] <- NULL
    obj[["n_depths"]] <- length(object)
    if (length(object) > 0) {
      depth_classes <- aaply(depths(object), 1, function(x){paste(x[1], '-', x[2], ' ', depths_units(object), sep='')}, .expand=FALSE)
      obj[["depth_classes"]] <- depth_classes
    }
#     n_horizons <- nrow(depths(object))
#     obj[["n_horizons"]] <- n_horizons
    obj[["id"]] = profile_id(object)
    obj[["units"]] = depths_units(object)
    
    # If there are horizon data in the object (SoilProfile and SoilProfileDataFrame classes)
    if ("horizons" %in% slotNames(object)) {
      if (nrow(horizons(object)) > 0)
	if (ncol(horizons(object)) > 1)
	  obj[["horizons"]] <- summary(horizons(object))
	else
	  obj[["horizons"]] <- summary(horizons(object)[[1]])
      else
	obj[["horizons"]] <- NULL
    }
    else 
      obj[["horizons"]] <- NULL

    # If there are site data in the object (SoilProfileDataFrame class)
    if ("site" %in% slotNames(object)) {
      if (ncol(object@site) > 0) 
	obj[["site"]] <- summary(object@site)
      else 
	obj[["site"]] <- NULL
    }
    else 
      obj[["site"]] <- NULL

    class(obj) <- "summary.Profile"
    obj
}

setMethod("summary", "summary.Profile", summary.Profile)

print.summary.Profile = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("ID: ", x[["id"]], "\n", sep="")
  cat("Number of horizons: ",  x[["n_depths"]], "\n", sep="")
  if (x[["n_depths"]] > 0) {
    cat("Available depths:\n")
    sapply(x[['depth_classes']], function(x){cat(x, '\n', sep='')})
  }
  if (!is.null(x[["horizons"]])) {
    cat("\nHorizons attributes:\n")
    print(x[["horizons"]])
  }
  if (!is.null(x[["site"]])) {
    cat("\nSampling site attributes:\n")
    print(x[["site"]])
  }

  invisible(x)
}

setMethod("print", "summary.Profile", print.summary.Profile)

## accessors

if (!isGeneric("depths"))
  setGeneric("depths", function(object, ...)
    standardGeneric("depths"))

setMethod("depths", "Profile",
  function(object, na.rm=TRUE){
    if (na.rm) {
      i <- which(!is.na(object@depths[, 1]) & !is.na(object@depths[, 2]))
      if (length(i) > 0)
        res <- object@depths[i, ]
      else
        res <- NULL
    }
    else
      res <- object@depths
    res
  }
)

if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

setMethod("profile_id", "Profile",
  function(object, na.rm=TRUE){
    if (na.rm)
      res <- object@id[!is.na(object@id)]
    else
      res <- object@id
    res
  }
)

if (!isGeneric("depths_units"))
  setGeneric("depths_units", function(object, ...)
    standardGeneric("depths_units"))

setMethod("depths_units", "Profile",
  function(object)
    object@depth_units
)

## overloads

# overload min() to give us the min depth within a profile
setMethod(f='min', signature='Profile',
definition=function(x)
    max(sapply(x@depths, min, na.rm=TRUE))
)

# overload max() to give us the max depth within a profile
setMethod(f='max', signature='Profile',
definition=function(x)
    max(sapply(x@depths, max, na.rm=TRUE))
)

# overload length() to give us the number of horizons
setMethod(f='length', signature='Profile',
  definition=function(x){
    res <- nrow(depths(x, na.rm=TRUE))
    if (is.null(res))
      res <- 0
    res
  }
)