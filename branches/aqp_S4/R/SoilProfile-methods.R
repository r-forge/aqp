.withWarnings <- FALSE

## initializer
##
#' Initialises a new Profile object.
#' 
#' Profile objects are storing very basic metadata about a soil profile: 
#' depths, id, and the units in which depths are expressed.
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param units the unit in which horizon depths are expressed
#'
"SoilProfile" <- function(depths=matrix(ncol=2), horizons=data.frame(), id=as.character(NA), units="cm"){
  if (missing(units) & .withWarnings)
    warning("unspecified depths units, centimeters are used")
  # if the id is not given, it is initilialized at character(1).
  if (is.na(id))
    id <- as.character(1)
  # the id *must* be of class character (this is in the class definition)
  if (!is.character(id))
    id <- as.character(id)
  # if there is only one horizon, we force it to be passed as a matric object to pass the validity test
  if (!is.matrix(depths))
    depths <- matrix(depths, ncol=2)
  # creation of the object (includes a validity check)
  new("SoilProfile", depths=depths, id=id, units=units, horizons=horizons)
}

## show

setMethod(
  f='show',
  signature='SoilProfile',
  definition=function(object){
    cat("Object of class ", class(object), "\n", sep = "")
    cat("ID: ", profile_id(object), "\n", sep="")
    cat("Number of horizons: ", length(object), "\n", sep="")
    if (length(object) > 0) {
      cat("Available depths:\n")
      depth_classes <- aaply(depths(object), 1, function(x){paste(x[1], '-', x[2], ' ', units(object), sep='')}, .expand=FALSE)
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

summary.SoilProfile <- function (object, ...){
    obj <- list()
    obj[["class"]] = class(object)
    if (length(object) > 0)
      obj[["depths"]] <- depths(object)
    else
      obj[["depths"]] <- NA
    obj[["n_depths"]] <- length(object)
    if (length(object) > 0) {
      d <- depths(object)
      if (!is.matrix(object))
	d <- matrix(d, ncol=2, dimnames=list(NULL, depthsnames(object)))
      depth_classes <- aaply(d, 1, function(x){paste(x[1], '-', x[2], ' ', units(object), sep='')}, .expand=FALSE)
      obj[["depth_classes"]] <- depth_classes
    }
#     n_horizons <- nrow(depths(object))
#     obj[["n_horizons"]] <- n_horizons
    obj[["id"]] = profile_id(object)
    obj[["units"]] = units(object)
    
    # If there are horizon data in the object (SoilProfile and SoilProfileDataFrame classes)
    if ("horizons" %in% slotNames(object)) {
      if (nrow(horizons(object)) > 0)
	if (ncol(horizons(object)) > 1)
	  obj[["horizons"]] <- summary(horizons(object))
	else
	  obj[["horizons"]] <- summary(horizons(object)[[1]])
      else
	obj[["horizons"]] <- data.frame()
    }
    else 
      obj[["horizons"]] <- data.frame()

    # If there are site data in the object (SoilProfileDataFrame class)
    if ("site" %in% slotNames(object)) {
      if (ncol(object@site) > 0) 
	obj[["site"]] <- summary(object@site)
      else 
	obj[["site"]] <- data.frame()
    }
    else 
      obj[["site"]] <- data.frame()

    class(obj) <- "summary.SoilProfile"
    obj
}

setMethod("summary", "summary.SoilProfile", summary.SoilProfile)

print.summary.SoilProfile = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("ID: ", x[["id"]], "\n", sep="")
  cat("Number of horizons: ",  x[["n_depths"]], "\n", sep="")
  if (x[["n_depths"]] > 0) {
    cat("Available depths:\n")
    sapply(x[['depth_classes']], function(x){cat(x, '\n', sep='')})
  }
  if (length(x[["horizons"]]) > 0) {
    cat("\nHorizons attributes:\n")
    print(x[["horizons"]])
  }
  if (length(x[["site"]]) > 0) {
    cat("\nSampling site attributes:\n")
    print(x[["site"]])
  }

  invisible(x)
}

setMethod("print", "summary.SoilProfile", print.summary.SoilProfile)

## coerce

as.data.frame.SoilProfile = function(object, ...){
  id <- matrix(rep(profile_id(object), length(object)), ncol=1, dimnames=list(NULL, idname(object)))
  d <- depths(object)
  h <- horizons(object)
  # if depth is a vector 
  if (is.integer(d) | is.numeric(d)) 
    d <- matrix(d, ncol=2, dimnames=list(NULL, depthsnames(object)))
  data.frame(id, d, h)
}

setAs("SoilProfile", "data.frame", function(from)
	as.data.frame.SoilProfile(from))

## accessors

if (!isGeneric("depths"))
  setGeneric("depths", function(object, ...)
    standardGeneric("depths"))

setMethod("depths", "SoilProfile",
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

if (!isGeneric("depthsnames"))
  setGeneric("depthsnames", function(object, ...)
    standardGeneric("depthsnames"))

setMethod("depthsnames", "SoilProfile",
  function(object) {
    d <- depths(object)
    if (!is.matrix(d) & (length(d) == 2))
      res <- names(d)
    else
      res <- dimnames(d)[[2]]
    res
  }
)

if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfile",
  function(object, na.rm=TRUE){
    if (na.rm)
      res <- object@id[!is.na(object@id)]
    else
      res <- object@id
    res
  }
)

# retrieves the id colname in the original dataframe
if (!isGeneric("idname"))
  setGeneric("idname", function(object, ...)
    standardGeneric("idname"))

setMethod("idname", "SoilProfile",
  function(object) 
    names(profile_id(object))
)

units.SoilProfile <- function(object)
  object@units

## horizon accessor
##

if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod("horizons", "SoilProfile",
#' Retrieves the horizon information within a profile
#'
  function(object) {
    if (nrow(object@horizons) > 0)
      res <- object@horizons #res <- data.frame(profile_id=profile_id(object), object@horizons)
    else
      res <- data.frame()
  res
  }
)

if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

setMethod("site", "SoilProfile",
  function(object)
    NULL
)

## overloads

# overload min() to give us the min depth within a profile
setMethod(f='min', signature='SoilProfile',
definition=function(x)
    min(sapply(x@depths, min, na.rm=TRUE))
)
  
# overload max() to give us the max depth within a profile
setMethod(f='max', signature='SoilProfile',
definition=function(x)
    max(sapply(x@depths, max, na.rm=TRUE))
)

# overload length() to give us the number of horizons
setMethod(f='length', signature='SoilProfile',
  definition=function(x){
    if ((length(depths(x)) == 2) & (is.integer(depths(x)) | is.numeric(depths(x)))) # only one horizon
      res <- 1
    else
      res <- nrow(depths(x, na.rm=TRUE))
    if (is.null(res))
      res <- 0
    res
  }
)

setMethod("$", "SoilProfile",
  function(x, name) 
    horizons(x)[[name]]
)

setReplaceMethod("$", "SoilProfile",
  function(x, name, value) {
    horizons(x)[[name]] <- value
    x
  }
)

setMethod("[[", c("SoilProfile", "ANY", "missing"),
  function(x, i, j, ...)
    x@horizons[[i]]
)

setReplaceMethod("[[", c("SoilProfile", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    x@horizons[[i]] <- value
    x
  }
)

setMethod("[", c("SoilProfile", "ANY", "ANY"),
  function(x, i, j, ...) {
    if (!missing(i)) { 
      d <- depths(x)[i, ]
      # if there is only one horizon, we force it to be passed as a matric object to pass the validity test
      if (is.numeric(d) | is.integer(d))
	d <- matrix(d, ncol=2, dimnames=list(NULL, depthsnames(x)))
      if (missing(j)) # selection on rows
	x <- SoilProfile(depths=d, id=profile_id(x), horizons=horizons(x)[i, ], units=units(x))
      else {# selection on rows *and* cols
	h <- horizons(x)[i, j]
	if (!is.data.frame(h))
	  h <- as.data.frame(h)
	x <- SoilProfile(depths=d, id=profile_id(x), horizons=h, units=units(x))
      }
    }
    else {
      if (!missing(j)) # selection on cols
	x <- SoilProfile(depths=depths(x), id=profile_id(x), horizons=horizons(x)[, j], units=units(x))
#     if (!missing(j)) # no selection
#       x <- x
    }
    x
  }
)

setReplaceMethod("[", c("SoilProfile", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    if (!(missing(i)) & (missing(j)))
      x@horizons[i, ] <- value
    if ((missing(i)) & (!missing(j)))
      x@horizons[, j] <- value
    if ((!missing(i)) & (!missing(j)))
      x@horizons[i, j] <- value
    if ((!missing(i)) & (!missing(j)))
      x@horizons <- value
    x
  }
)


names.SoilProfile <- function(x) names(horizons(x))

"names<-.SoilProfile" <- function(x, value) {
  names(horizons(x)) <- value
  x
}

## Subset SP with a subset/select query on the horizon data AND/OR the depths

subset.SoilProfile <- function(x, subset, select, drop = FALSE, ...) {
  # adapted from subset.data.frame
  df <- as.data.frame(x)
  id <- idname(x)
  dpth <- depthsnames(x)

  # subset rows
  if (missing(subset))
        r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, df, parent.frame())
    if (!is.logical(r))
      stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }

  # select cols
  if (missing(select))
    vars <- setdiff(names(df), c(id, dpth))
  else {
    nl <- as.list(seq_along(df))
    names(nl) <- names(df)
    vars <- eval(substitute(select), nl, parent.frame())
  }

  # subset DF and create SPC
  sp <- df[r, c(id, dpth, vars)]
  # remove unused factors
  sp <- droplevels(sp)
  depths(sp) <- c(id, dpth)
 
  sp
}