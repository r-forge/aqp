## initializer
"Profile" <- function(depths=matrix(ncol=2), horizons=data.frame(), id=as.character(NA), depth_units="cm"){
  if (ncol(depths) != 2)
    stop("unconsistent number of depths columns")
  if (!all(is.na(depths))) {
    if (any((depths[, 2] - depths[, 1]) < 0))
      stop("the depths matrix must be ordered with top depths first, and bottom depths second")
  }
  if (is.na(id))
    id <- as.character(1)
  colnames(depths) <- c("top", "bottom")
  new("Profile", depths=depths, horizons=horizons, id=id, depth_units=depth_units)
}

## show
setMethod(
  f='show',
  signature='Profile',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("ID: ", profile_id(object), "\n", sep="")
    cat("Number of horizons: ", nrow(horizons(object)), "\n", sep="")
    if (length(object) > 0) {
      cat("Available depths:\n")
      depth_classes <- aaply(depths(object), 1, function(x){paste(x[1], '-', x[2], ' ', depths_units(object), sep='')}, .expand=FALSE)
      sapply(depth_classes, function(x){cat(x, "\n", sep="")})
    }
    if (nrow(horizons(object)) > 0) {
      cat("Horizons attributes:\n")
      print(horizons(object))
    }
  }
)

## summary
if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...)
    standardGeneric("summary"))

summary.Profile <- function (object, ...){
    obj = list()
    obj[["class"]] = class(object)
    if (length(object) > 0)
      obj[["depths"]] <- depths(object)
    else
      obj[["depths"]] <- NULL
    obj[["n_depths"]] <- length(object)
    if (length(object) > 0) {
      depth_classes <- aaply(depths(object), 1, function(x){paste(x[1], '-', x[2], ' ', depths_units(object), sep='')}, .expand=FALSE)
      obj[["depth_classes"]] <- sapply(depth_classes, function(x){cat(x, "\n", sep="")})
    }
    n_horizons <- nrow(horizons(object))
    obj[["n_horizons"]] <- n_horizons
    if ( n_horizons > 0)
      if (ncol(horizons(object)) > 1)
        obj[["horizons"]] <- summary(horizons(object))
      else
        obj[["horizons"]] <- summary(horizons(object)[[1]])
    else
      obj[["horizons"]] <- NULL
    obj[["id"]] = profile_id(object)
    obj[["units"]] = depths_units(object)
    class(obj) = "summary.Profile"
    obj
}

setMethod("summary", "summary.Profile", summary.Profile)

print.summary.Profile = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("ID: ", x[["id"]], "\n", sep="")
  cat("Number of horizons: ",  x[["n_horizons"]], "\n", sep="")
  if (x[["n_depths"]] > 0) {
    cat("Available depths:\n")
    for (i in 1:length(x[["depth_classes"]]))
      cat(x[["depth_classes"]][i], "\n", sep="")
  }
  if (x[["n_horizons"]] > 0) {
    cat("Horizons attributes:\n")
    print(x[["horizons"]])
  }
  invisible(x)
}

setMethod("print", "summary.Profile", print.summary.Profile)

## accessors
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod("horizons", "Profile",
  function(object)
    object@horizons
)

if (!isGeneric("depths"))
  setGeneric("depths", function(object, ...)
    standardGeneric("depths"))

setMethod("depths", "Profile",
  function(object, na.rm=TRUE){
    if (na.rm) {
      i <- which(!is.na(object@depths))
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
  function(object)
    object@id
)

if (!isGeneric("depths_units"))
  setGeneric("depths_units", function(object, ...)
    standardGeneric("depths_units"))

setMethod("depths_units", "Profile",
  function(object)
    object@depth_units
)

## overloads

# overload length() to give us the number of horizons
setMethod(f='length', signature='Profile',
  definition=function(x){
    res <- nrow(depths(x, na.rm=TRUE))
    if (is.null(res))
      res <- 0
    res
  }
)