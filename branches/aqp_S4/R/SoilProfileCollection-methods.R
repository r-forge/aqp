#' SoilProfileCollection objects are storing...
#'
#' @param depths a two-columns matrix with the top and bottom depths of each horizon in the profile.
#' @param id a unique identification of the profile
#' @param units the unit in which horizon depths are expressed
#'
"SoilProfileCollection" <- function(profiles=list(SoilProfile()), site=data.frame(), ids=as.character(NA)){
  # default is that the ids are the id of the SoilProfile objects
  if (all(is.na(ids)))
    ids <- laply(profiles, profile_id)
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
      cat("Depth range: ", min(object), "-", max(object), " ", units(object), "\n", sep="")
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
	obj[['horizons']] <- summary(horizons(object, keep.id=FALSE))
      }
    }
    else # no profile in the collection
      obj[["n_horizons_data"]] <- NA
    if (length(site(object)) > 0)
      obj[["site"]] <- summary(site(object))
    else
      obj[["site"]] <- NA
    obj[["units"]] <- units(object)
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
.getProfilesAsList <- function(object, id=NA){
  if (!is.list(profiles(object, id=id)))
    profiles_list <- list(profiles(object, id=id))
  else 
    profiles_list <- profiles(object, id=id)
  profiles_list
}

## coerce

as.data.frame.SoilProfileCollection = function(object, ...) {
  ids <- unlist(llply(profiles(object), 
    .fun=function(x)rep(profile_id(x), length(x))
    ), use.names=FALSE)

  ids <- matrix(ids, ncol=1, dimnames=list(NULL, idname(object)))
  d <- do.call(rbind, depths(object))
  row.names(d) <- 1:nrow(d)
  h <- horizons(object, keep.id=FALSE)
  
  if (length(site(object)) == 0)
    res <- data.frame(ids, d, h)
  else {
    n_horizons <- llply(profiles(object), length)
    site <- sapply(site(object), function(x){rep(x, n_horizons)})
    res <- data.frame(ids, d, site, h)
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
  function(object, keep.id=FALSE) {
    if (keep.id) {
      res <- data.frame(profile_id(object), object@site)
      names(res)[1] <- idname(object)
    }
    else {
      res <- object@site
    }
    if (length(res) > 1)
      rownames(res) <- profile_id(object)
    else
      if ((length(res) == 1) & !is.data.frame(res))
	names(res) <- profile_id(object)

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

# gives the name of the depths columns used
# in the original data.frame
if (!isGeneric("depthsnames"))
  setGeneric("depthsnames", function(object, ...)
    standardGeneric("depthsnames"))

setMethod("depthsnames", "SoilProfileCollection",
  function(object) {
    dn <- laply(.getProfilesAsList(object), depthsnames)
    res <- apply(dn, 2, unique)
    # if there is only one set of depthsnames, we remove the 
    # headers from the result to get just a plain character vector.
    if (is.null(nrow(res)))
      names(res) <- NULL
    res
  }
)

# returns a data.frame aggregating horizons data
if (!isGeneric("horizons"))
  setGeneric("horizons", function(object, ...)
    standardGeneric("horizons"))

setMethod(f='horizons', signature='SoilProfileCollection',
  function(object ,id=as.numeric(NA), keep.id=TRUE){
    if (all(is.na(id))) { # if no profile id, the data for every profile is returnedt)
      res <- ldply(.getProfilesAsList(object), horizons)
    }
    else {
      if (!is.numeric(id))
	id <- which(profile_id(object) %in% id)
      res <- ldply(profiles(object)[id], horizons)
    }
    nm <- names(res)
    # option to remove the .id column ldply is adding
    idx <- which(names(res) == '.id')
    if (!keep.id & (length(idx) != 0)) {
      res <- res[, -idx] # dirty hack to remove the .id column ldply is adding
      if (is.null(ncol(res))) {# if it has become  a vector 
	res <- data.frame(res)
	names(res) <- nm[-idx]
      }
    }
    else # if we keep it we do rename it
      if (keep.id)
	names(res)[idx] <- idname(object)
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

units.SoilProfileCollection <- function(object)
  unique(laply(.getProfilesAsList(object), units))

# retrieves the ids
if (!isGeneric("profile_id"))
  setGeneric("profile_id", function(object, ...)
    standardGeneric("profile_id"))

setMethod("profile_id", "SoilProfileCollection",
  function(object) 
    object@ids
)

# retrieves the id colname in the original dataframe
if (!isGeneric("idname"))
    setGeneric("idname", function(object, ...)
      standardGeneric("idname"))

setMethod("idname", "SoilProfileCollection",
  function(object) 
    unique(laply(profiles(object), function(x) names(profile_id(x))))
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
    length(.getProfilesAsList(x))
  }
)

setMethod("$", "SoilProfileCollection",
  function(x, name) {
    if (name %in% names(horizons(x)))
      res <- horizons(x)[[name]]
    else
      if (name %in% names(site(x)))
	res <- site(x)[[name]]
      else
	res <- NULL
    res
  }
)

setReplaceMethod("$", "SoilProfileCollection",
  function(x, name, value) {
    if (name %in% names(horizons(x)))
      horizons(x)[[name]] <- value
    else
      site(x)[[name]] <- value
    x
  }
)

setMethod("[[", c("SoilProfileCollection", "ANY", "missing"),
  function(x, i, j, ...) {
    if (i %in% names(horizons(x)))
      res <- horizons(x)[[i]]
    else
      if (i %in% names(site(x)))
	res <- site(x)[[i]]
      else
	res <- NULL
    res
  }
)

setReplaceMethod("[[", c("SoilProfileCollection", "ANY", "missing", "ANY"),
  function(x, i, j, value) {
    if (i %in% names(horizons(x)))
      horizons(x)[[i]] <- value
    else
      site(x)[[i]] <- value
    x
  }
)

setMethod("[", c("SoilProfileCollection", "ANY", "ANY"),
  function(x, i, j, ...) {
    
    # selection of profiles
    if (!missing(i)) { 
      p <- .getProfilesAsList(x, i)
      ids <- profile_id(x)[i]
      if (length(site(x)) > 0)
	s <- site(x)[i, ]
      else
	s <- data.frame() 
      # creation of a SPC object
      x <- SoilProfileCollection(profiles=p, site=s, ids=ids)

      # adding cols selection
      if (!missing(j)) {
	# if theres some site data it gets confusing (what should we subset?)
	if (length(site(x)) > 0)
	  warning('site data is present')
	# we subset horizon data
	h <- horizons(x, keep.id=FALSE)[, j]
	if (!is.data.frame(h)) {
	  h <- as.data.frame(h)
	  names(h) <- names(horizons(x))[j]
	  cat('\n/!\\ this is a bug, not a feature :(\n')
	}
	horizons(x) <- h
      }
    }
    # no profiles selection
    else {
      if (!missing(j)) {
	# if theres some site data it gets confusing (what should we subset?)
	if (length(site(x)) > 0)
	  warning('site data is present')
	# we subset horizon data
	h <- horizons(x, keep.id=F)[, j]
	if (!is.data.frame(h)) {
	  h <- as.data.frame(h)
	  names(h) <- names(horizons(x))[j]
	}
	horizons(x) <- h
      }
    }
    x
  }
)

# getting names
names.SoilProfileCollection <- function(x) c(names(horizons(x)), names(site(x)))

## data manipulation

## Subset SPC with a subset/select query on the horizon data AND/OR the depths AND/OR site data

subset.SoilProfileCollection <- function(x, subset, select, drop = FALSE, ...) {
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
  spc <- df[r, c(id, dpth, vars)]
  # remove unused factors
  spc <- droplevels(spc)
  depths(spc) <- c(id, dpth)
 
  # if there is site data, the subset is also applied
  if (length(site(x)) > 0) {
    s <- names(site(x)) # retrieves the col names of the site data
    if (any(s %in% names(spc)))
	i <- which(s %in% names(spc))
	site(spc) <- s[i]
  }
  
  spc
}

# adds SoilProfiles to the collection
if (!isGeneric("add"))
  setGeneric("add", function(x, y, ...)
    standardGeneric("add"))

# Adding 2 SPC together
setMethod("add", signature=c("SoilProfileCollection", "SoilProfileCollection"),
  function(x, y, ...) { # x and y are SPC
    if (identical(x, y))
      stop('you cant add an object to itself')
    # checking ID unicity
    if (any(profile_id(y) %in% profile_id(x))) {
      i <- which(profile_id(y) %in% profile_id(x))
      # replace duplicated IDs by unique IDs
      ids <- make.unique(c(profile_id(x), profile_id(y)[i]))
      profile_id(y)[i] <- ids[length(profile_id(x))+1:length(ids)]
    }
    profile_ids <- c(profile_id(x), profile_id(y))
    profiles_list <- c(.getProfilesAsList(x), .getProfilesAsList(y))
    
    sx <- site(x)
    sy <- site(y)
    # case 1: both have site data
    if ((length(sx) > 0) & (length(sy) > 0)) {
      tmp_id <- .createCharHash(n=5, prefix='TMP') 
      sx[, tmp_id] <- rownames(sx)
      sy[, tmp_id] <- rownames(sy)
      site_data <- join(sx, sy, by=tmp_id, type='full')
      site_data[, tmp_id] <- NULL
    }
    # case 2 & 3: only one object has site data
    if ((length(sx) > 0) & (length(sy) == 0))
      site_data <- rbind(sx, matrix(NA, ncol=ncol(sx), nrow=length(y), dimnames=list(NULL, names(sx))))
    if ((length(sx) == 0) & (length(sy) > 0))
      site_data <- rbind(sy, matrix(NA, ncol=ncol(sy), nrow=length(x), dimnames=list(NULL, names(sy))))
    # case 4: none has site data
    if ((length(sx) == 0) & (length(sy) == 0))
      site_data <- data.frame()
    res <- SoilProfileCollection(profiles=profiles_list, site=site_data, ids=profile_ids)
    res
  }
)

# Adding a SP to a SPC
setMethod("add", signature=c("SoilProfileCollection", "SoilProfile"),
  function(x, y, ...) {
    # x is a SPC, y is a SP
    # checking ID unicity
    if (profile_id(y) %in% profile_id(x)) {
      profile_id(y) <- make.unique(c(profile_id(x), profile_id(y)))[length(x)+1]
    }
    profiles_list <- .getProfilesAsList(x)
    profiles_list <- c(profiles_list, y)
    
    if (length(site(x)) > 0) { # if some site data is present
      # if the added SP has matching colnames
      if (any(names(horizons(x)) %in% names(site(x)))) {
	i <- names(horizons(x)) %in% names(site(x))
	df <- horizons(sp)[,which(i)]
      }
      # otherwise we create NA values
    }
    else 
      site_data <- data.frame()

    res <- SoilProfileCollection(profiles=profiles_list, site=site_data)
    res
  }
)

# Adding 2 SP together to form a new SPC