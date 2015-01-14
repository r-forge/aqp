require(aqp)
require(stringr)
require(plyr)

## Methods associated with classes described in classes.r
##

## initialisation of objects
##

"SoilProfile" <- function(
    id = as.character(NA),
    depths = matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c('top', 'bottom'))),
    depth_units = 'cm',
    sp = new('SpatialPoints'),
    horizons = data.frame(),
    site = data.frame()
  ) {
  new(
    "SoilProfile", 
    id = id, 
    depths = depths, 
    depth_units = depth_units,
    sp = sp,
    horizons = horizons,
    site = site
  )
}

"SPC" <- function(
  ...,
  profiles = list(SoilProfile())
){
  
  dots <- list(...)
  
  # if some of the dots are giving a SoilProfile
  if (any(sapply(dots, inherits, "SoilProfile"))) {
    # Select SoilProfile objects
    idx_sprof <- which(sapply(dots, inherits, "SoilProfile"))    
    sprofs <- dots[[idx_sprof]]
    
    # Retrieve IDs of the listed SoilProfile
    ids <- do.call("c", lapply(sprofs, function(x) x@id))
    
    # Testing unicity of IDs
    if (any(duplicated(ids))) {
      warning("Duplicated SoilProfile IDs. Re-building IDs from scratch.")
      ids <- as.character(1:length(ids))
      names(sprofs) <- ids
    }
    
    # Building object from results
    obj <- new("SPC", profiles = sprofs)
    
  } else {
    obj <- new("SPC", profiles = profiles)
  }
  
  return(obj)
}

## Parsing formula interface
##

.parse_formula <- function(formula, object){
  formula <- str_c(deparse(formula, 500), collapse = "")
  
  elements <- str_split(formula, fixed("~"))[[1]]
  n_elements <- length(elements)
  length_elements <- lapply(elements, str_length)
  elements <- lapply(str_split(elements, "[+*]"), str_trim)
  
  # Replacing any void elemnt by NULL
  elements <- lapply(elements, function(x) 
    if(any(lapply(x, str_length) == 0)) x = NULL
    else x
  )
  
#   if (n_elements == 1) {
#     cols_id <- NULL
#     cols_depths <- elements[[1]]
#     cols_hz <- NULL
#   }
#   else
  if (n_elements == 2) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- cols_site <- NULL
  }
  else if (n_elements == 3) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- elements[[3]]
    cols_site <- NULL
  }
  else if (n_elements == 4) {
    cols_id <- elements[[1]]
    cols_depths <- elements[[2]]
    cols_hz <- elements[[3]]
    cols_site <- elements[[4]]
  }
  else {
    stop('wrong formula.')
  }
  
  # Sanity check: at this point ID can't be NULL
  if (is.null(cols_id)) stop('wrong formula')

  list(id = cols_id, depths = cols_depths, horizons = cols_hz, site = cols_site)
}

## depths<- setter method - to create SP/SPC objects
## 
## 1. puts the rest of stuff in @horizons
## depths(foo) <- id ~ top + bottom 
##
## 2. puts **only** x and y in @horizons and discards the rest
## depths(foo) <- id ~ top + bottom ~ x + y 
##
## 3. puts **only** x and y in @horizons, z in @site, and discards the rest
## depths(foo) <- id ~ top + bottom ~ x + y ~ z
##
## 4. puts **only** x and y in @site, and the rest in @horizons
## depths(foo) <- id ~ top + bottom ~ ... ~ x + y
##
if (!isGeneric('depths<-'))
  setGeneric('depths<-', function(object, value) standardGeneric('depths<-'))

## Method that creates SPC from a data.frame
##
setReplaceMethod("depths", "data.frame",
  function(object, value) {      
      # Retrieve variables names
      nm.vars <- .parse_formula(value, object)
      
      # If no horizon data has been given we put all the leftover data
      if (is.null(nm.vars$horizons)) {
        nm.vars$horizons <- names(object)[! names(object) %in% c(nm.vars$id, nm.vars$depths, nm.vars$site)]
      }
      
      lst_df <- dlply(object, nm.vars$id, identity)
      
      # Create list of SoilProfile
      lst_sp <- llply(lst_df, function(x) {
        
        cur_id <- as.character(unique(x[[nm.vars$id]]))
        names(cur_id) <- nm.vars$id
        depths <- as.matrix(x[, nm.vars$depths])
        sp <- new('SpatialPoints')
        horizons <- x[, nm.vars$horizons, drop = FALSE]
        
        if(is.null(nm.vars$site)) site <- data.frame()
        else site <- x[, nm.vars$site, drop = FALSE]
        
        new("SoilProfile", id = cur_id, depths = depths, sp = sp, horizons = horizons, site = site)
      })
      
    SPC(profiles = lst_sp)
  }
)

# setReplaceMethod("depths", "SpatialPoints",
#   function(object, value) {
#     ## depths(my_spdf) <- id ~ top + bottom
#     ##
#     NULL
#   }
# )

## Get IDs
##
if (!isGeneric('ids'))
  setGeneric('ids', function(object) 
    standardGeneric('ids'))

setMethod("ids", "SoilProfile",
          function(object) {
            object@id
          }
)

setMethod("ids", "SPC",
          function(object) {
            laply(object@profiles, ids)
          }
)

## Get depths
##
if (!isGeneric('depths'))
  setGeneric('depths', function(object) 
    standardGeneric('depths'))

setMethod("depths", "SoilProfile",
          function(object) {
            object@depths
          }
)

setMethod("depths", "SPC",
          function(object) {
            lapply(object@profiles, depths)
          }
)

if (!isGeneric('depth_units'))
  setGeneric('depth_units', function(object) 
    standardGeneric('depth_units'))

setMethod("depth_units", "SoilProfile",
          function(object) {
            object@depth_units
          }
)

setMethod("depth_units", "SPC",
          function(object) {
            unique(laply(object@profiles, depth_units))
          }
)

## Get list or unique SoilProfile
##
# if (!isGeneric('profiles'))
setGeneric('profiles', function(object, i = NULL) 
  standardGeneric('profiles'))

setMethod("profiles", "SPC",
          function(object, i = NULL) {
            if (is.null(i)) object@profiles
            else object@profiles[[i]]
          }
)

## Get horizons data
##
if (!isGeneric('horizons'))
  setGeneric('horizons', function(object, ...) 
    standardGeneric('horizons'))

setMethod("horizons", "SoilProfile",
          function(object) {
            object@horizons
          })

setMethod("horizons", "SPC",
          function(object, as.list = FALSE) {
            res <- lapply(object@profiles, function(x) x@horizons)
            if (!as.list) {
              res <- data.frame(do.call('rbind', res), row.names = NULL)
            }
            res
          })

## Get horizons data
##
if (!isGeneric('site'))
  setGeneric('site', function(object, ...) 
    standardGeneric('site'))

setMethod("site", "SoilProfile",
          function(object) {
            object@site
          })

setMethod("site", "SPC",
  function(object, as.list = FALSE) {
    res <- lapply(profiles(object), site)
    if (!as.list) {
      res <- data.frame(do.call('rbind', res), row.names = NULL)
    }
    res
  }
)

if (!isGeneric('site<-'))
  setGeneric('site<-', function(object, ...) 
    standardGeneric('site<-'))

setReplaceMethod("site", "SoilProfile",
 function(object, value) {
   
   # creation of site data from horizon data
   if (inherits(value, "formula")) {
     mf <- model.frame(value, horizons(object), na.action = na.pass)
     new_site <- unique(mf)
   }
   
   # Throw error if more than one line
   if (nrow(new_site) > 1) stop('site data error')

   # Affect data to site slot
   object@site <- new_site
   
   # Remove data from horizons slot
   object@horizons <- object@horizons[, -1 * which(names(object@horizons) %in% names(new_site)), drop = FALSE]
     
   return(object)
 }
)

setReplaceMethod("site", "SPC",
  function(object, value) {
    lspc <- lapply(profiles(object), function(x) {
        site(x) <- value
        x
      })
    SPC(profiles = lspc)
  }
)

## Various overloads
##

# Number of profiles in the collection
length.SPC <- function(x) length(profiles(x))

# Number of horizons
setMethod("nrow", "SoilProfile",
          function(x) {
            nrow(x@depths)
          }
)

setMethod("nrow", "SPC",
          function(x) {
            sum(laply(profiles(x), nrow))
          }
)


# Minimum depth
setMethod("min", "SoilProfile",
          function(x, na.rm = TRUE) {
            min(x@depths[, 1])
          }
)

setMethod("min", "SPC",
          function(x, na.rm = TRUE) {
            min(laply(profiles(x), min))
          }
)

# Maximum depth
setMethod("max", "SoilProfile",
          function(x, na.rm = TRUE) {
            max(x@depths[, 2])
          }
)

setMethod("max", "SPC",
          function(x, na.rm = TRUE) {
            max(laply(profiles(x), max))
          }
)

setMethod("names", "SoilProfile",
  function(x) {
    c(names(horizons(x)), names(site(x)))
  }
)

setMethod("names", "SPC",
  function(x) {
    c(names(horizons(x)), names(site(x)))
  }
)


## Attributes access

setMethod(
  f = "$", 
  signature = "SoilProfile",
  definition = function(x, name) {
    
    # get names from site and hz data
    s.names <- names(site(x))
    h.names <- names(horizons(x))
    
    # when site data are initialized from an external DF, it is possible that
    # there will be duplicate column names
    if(name %in% h.names & name %in% s.names)
      warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
    
    # get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]
    
    # otherwise check site data
    else
      if (name %in% s.names)
        res <- site(x)[[name]]
    
    # if still missing return NULL
    else
      res <- NULL
    
    return(res)
  }
)

setMethod(
  f = "$", 
  signature = "SPC",
  definition = function(x, name) {
    
    # get names from site and hz data
    s.names <- names(site(x))
    h.names <- names(horizons(x))
    
    # when site data are initialized from an external DF, it is possible that
    # there will be duplicate column names
    if(name %in% h.names & name %in% s.names)
      warning('column name is present in horizon and site data, extracting from horizon data only', call.=FALSE)
    
    # get column from horizon data
    if (name %in% h.names)
      res <- horizons(x)[[name]]
    
    # otherwise check site data
    else
      if (name %in% s.names)
        res <- site(x)[[name]]
    
    # if still missing return NULL
    else
      res <- NULL
    
    return(res)
  }
)

## Print and summary

setMethod(
  f = 'show',
  signature = 'SoilProfile',
  definition = function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Soil depth range: ", min(object),"-", max(object), " ", depth_units(object), "\n", sep = "")
    cat(ncol(horizons(object))," horizon attributes, ", ncol(site(object))," site attributes\n", sep = "")
  }
)

setMethod(
  f = 'show',
  signature = 'SPC',
  definition = function(object){
    cat(paste("Object of class ", class(object), "\n", sep = ""))
    cat("Collection of ", length(object)," soil profiles\n", sep='')
    cat("Soil depth range: ", min(object),"-", max(object), " ", depth_units(object), "\n", sep = "")
    cat(ncol(horizons(object))," horizon attributes, ", ncol(site(object))," site attributes\n", sep = "")
  }
)

summary.SoilProfile <- function (object, ...){
  obj <- list()
  obj[["class"]] <- class(object)
  obj[["depth_range"]] <- c(min(object), max(object))
  obj[["depth_units"]] <- depth_units(object)
  
#   sp_data <- ncol(coordinates(object@sp)) >= 2
  n_hz_data <- ncol(horizons(object))
  n_site_data <- ncol(site(object))
  
  if (n_hz_data > 1) {
      obj[["hz_data"]] <- summary(horizons(object))
  } else obj[["hz_data"]] <- NULL

  if (n_site_data > 1) {
    obj[["site_data"]] <- site(object)
  } else obj[["site_data"]] <- NULL

  class(obj) = "summary.SoilProfile"
  obj
}

summary.SPC <- function (object, ...){
  obj <- list()
  obj[["class"]] <- class(object)
  obj[["depth_units"]] <- depth_units(object)
  obj[["depth_range"]] <- c(min(object), max(object))
  obj[["length"]] <- length(object)
  
  #   sp_data <- ncol(coordinates(object@sp)) >= 2
  n_hz_data <- ncol(horizons(object))
  n_site_data <- ncol(site(object))
  
  if (n_hz_data > 1) {
    obj[["hz_data"]] <- summary(horizons(object))
} else obj[["hz_data"]] <- NULL

if (n_site_data > 1) {
  obj[["site_data"]] <- summary(site(object))
} else obj[["site_data"]] <- NULL

class(obj) = "summary.SPC"
obj
}

print.summary.SoilProfile = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Soil depth range: ", x[["depth_range"]][1], "-", x[["depth_range"]][2], " ", x[["depth_units"]], "\n", sep = "")
  
  cat("\n")
  
  if (!is.null(x[["hz_data"]])) {
    cat(ncol(x[["hz_data"]]), " horizon attributes:\n")
    print(x[["hz_data"]])
    cat("\n")
  } else {
    cat("No horizon attributes.\n")
  }
  
  if (!is.null(x[["site_data"]])) {
    cat(ncol(x[["site_data"]]), " horizon attributes:\n")
    print(x[["site_data"]])
    cat("\n")
  } else {
    cat("No site attributes.\n")
  }
  
  invisible(x)
}

print.summary.SPC = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Collection of ", x[["length"]]," soil profiles\n", sep='')
  cat("Soil depth range: ", x[["depth_range"]][1], "-", x[["depth_range"]][2], " ", x[["depth_units"]], "\n", sep = "")
  
  cat("\n")
  
  if (!is.null(x[["hz_data"]])) {
    cat(ncol(x[["hz_data"]]), " horizon attributes:\n")
    print(x[["hz_data"]])
    cat("\n")
  } else {
    cat("No horizon attributes.\n")
  }
  
  if (!is.null(x[["site_data"]])) {
    cat(ncol(x[["site_data"]]), " horizon attributes:\n")
    print(x[["site_data"]])
    cat("\n")
  } else {
    cat("No site attributes.\n")
  }
  
  invisible(x)
}


## Coercition methods

as.data.frame.SoilProfile <- function(x, ...)  {
  
  # ID (always defined)
  df_id <- data.frame(rep(x@id, times = nrow(x)))
  names(df_id) <- names(x@id)
  
  # Depths (always defined)
  df_depths <- data.frame(x@depths)
  
  # Spatial point
  if (.hasSpatialPoint(x)) {
    df_sp <- as.data.frame(coordinates(x@sp))
    # replicate coordinates for each hz
    df_sp <- ldply(1:nrow(x), function(x) df_sp)
  } else {
    df_sp <- NULL
  }
  
  # Horizon data
  if (nrow(horizons(x)) > 0) {
    df_hz <- horizons(x)
  } else {
    df_hz <- NULL
  }
  
  # Site data
  if (nrow(site(x)) > 0) {
    df_site <- site(x)
    # replicate site data for each hz
    df_site <- ldply(1:nrow(x), function(x) df_site)
  } else {
    df_site <- NULL
  }
  
  l_df <- list(df_id, df_depths, df_sp, df_hz, df_site)
  # Eliminate NULL fields
  l_df <- l_df[which(laply(l_df, function(x) !is.null(x)))]
    
  do.call('cbind', c(l_df, row.names = NULL))
}

setAs("SoilProfile", "data.frame", function(from)
  as.data.frame.SoilProfile(from))

as.data.frame.SPC <- function(x, ...) {
  data.frame( do.call('rbind', lapply(profiles(x), as.data.frame)) , row.names = NULL)
}

setAs("SPC", "data.frame", function(from)
  as.data.frame.SPC(from))

as.SoilProfileCollection.SPC <- function(from) {
  # Get col names of SPC slots
  nm <- list(
    id = unique(laply(profiles(from), function(x) names(x@id))),
    depths = colnames(do.call(rbind, depths(from))),
    site = names(site(from)),
    sp = colnames(coordinates(from)),
    p4s = proj4string(from)
  )
  
  .depths_aqp <- getMethod('depths<-', 'data.frame', "package:aqp")
  .site_aqp <- getMethod('site<-', 'SoilProfileCollection', "package:aqp")
  f_depths <- as.formula(paste(nm$id, "~", paste(nm$depths, collapse = "+")))
  spc <- .depths_aqp(as.data.frame(from), f_depths)
    
  # Initiate site slot if present
  if (length(nm$site) > 0) {
    f_site <- as.formula(paste("~", paste(nm$site, collapse = "+")))
    spc <- .site_aqp(spc, f_site)
  }
  
  # Initiate sp slot if present
  if (length(nm$sp) > 0) {
    .coordinates_aqp <- getMethod('coordinates<-', 'SoilProfileCollection', "package:aqp")
    .proj4string_aqp <- getMethod('proj4string<-', 'SoilProfileCollection', "package:aqp")
    
    f_sp <- as.formula(paste("~", paste(nm$sp, collapse = "+")))
    # Put coords in site data
    spc <- .site_aqp(spc, as.formula(paste("~", paste(nm$sp, collapse = "+"))))
    # Set coordinates and proj4string
    spc <- .coordinates_aqp(spc, f_sp)
    spc <- .proj4string_aqp(spc, proj4string(from))
  }
  
  spc
}

setAs("SPC", "SoilProfileCollection", function(from)
  as.SoilProfileCollection.SPC(from))

## 
## Spatial stuff
## 

# Utility function that tests if 
# a SoilProfile has spatial coordinates
.hasSpatialPoint <- function(x) !identical(x@sp, new('SpatialPoints'))

# proj4string

setMethod(f = 'proj4string', signature = 'SoilProfile',
  function(obj){
    obj@sp@proj4string@projargs
  }
)

setMethod(f = 'proj4string', signature = 'SPC',
  function(obj){
    unique(unlist(lapply(profiles(obj), proj4string)))
  }
)

setReplaceMethod("proj4string", "SoilProfile",
  function(obj, value) {
    proj4string(obj@sp) <- value
    obj
  }
)

setReplaceMethod("proj4string", "SPC",
 function(obj, value) {
   obj <- lapply(profiles(obj), function(x){
     proj4string(x) <- value
     x
   })
   
   SPC(profiles = obj)
 }
)

# coordinates

setMethod("coordinates", "SoilProfile",
  function(obj) {
    res <- coordinates(obj@sp)
    # over-writing the default sp::coordinates behaviour: we
    # return NULL if no coordinates
    if (ncol(res) < 2) res <- NULL
    res
  }
)

setMethod("coordinates", "SPC",
function(obj) {
  do.call(rbind, lapply(profiles(obj), coordinates))
}
)

setReplaceMethod("coordinates", "SoilProfile",
  function(object, value) {
    
    # basic sanity check... needs work
    if(! inherits(value, "formula"))
      stop('invalid formula', call. = FALSE)
    
    # extract coordinates as matrix
    mf <- model.frame(value, site(object), na.action = na.pass)
    nm <- names(mf)
    mf <- data.matrix(mf, rownames.force = FALSE)
    
    # test for missing coordinates
    mf.missing <- apply(mf, 2, is.na)
    
    if(any(mf.missing))
      stop('cannot initialize a SpatialPoints object with missing coordinates', call. = FALSE)
    
    # Instanciate the sp slot
    object@sp <- SpatialPoints(coord = mf)
    
    # Remove coordinates from site slot
    object@site <- object@site[,-1 * which(names(object@site) %in% nm), drop = FALSE]
    
    object
  }
)

setReplaceMethod("coordinates", "SPC",
  function(object, value) {
   
    lspc <- lapply(profiles(object), function(x) {
      coordinates(x) <- value
      x
    })
        
    SPC(profiles = lspc)
    
  }
)

## Get SpatialPoints
##

setAs("SoilProfile", "SpatialPoints", 
  function(from) {
    if (.hasSpatialPoint(from)){
      sp <- from@sp
    } else {
      sp <- NULL
    }
    sp
  }
)

setAs("SPC", "SpatialPoints", 
  function(from) {
    do.call("rbind", lapply(profiles(from), function(x) as(x, "SpatialPoints")))
  }
)

setAs("SoilProfile", "SpatialPointsDataFrame", 
      function(from) {
        sp <- as(from, "SpatialPoints")
        
        if (is.null(sp)) spdf <- NULL
        else spdf <- SpatialPointsDataFrame(sp, data = site(from))
        
        spdf
      }
)

setAs("SPC", "SpatialPointsDataFrame", 
      function(from) {
        sp <- as(from, "SpatialPoints")
        
        if (is.null(sp)) spdf <- NULL
        else spdf <- SpatialPointsDataFrame(sp, data = site(from))
        
        spdf
      }
)

# Extract site data from covariates

extract_covariates <- function(spc, cov) {
  
  # Get spatial points
  sp <- as(spc, "SpatialPoints")
  
  if(inherits(cov, "Raster")) {
    
    df <- extract(x = cov, y = sp)
    
    # If this i a RasterLayer
    if (nlayers(cov) < 2) {
      df <- data.frame(df)
      names(df) <- names(cov)
    } else {
      # Just coerce matrix to data.frame
      df <- data.frame(df)
    }
  } else if(inherits(cov, "Spatial")) {
    df <- sp %over% cov
  } else stop('invalid covariate')
  
  df
}

add_covariates <- function(spc, cov) {
  
  df <- extract_covariates(spc, cov)
  
  lsp <- lapply(1:length(spc), function(x){
    p <- profiles(spc, x)
    
    if (nrow(site(p)) == 0) {
      p@site <- df[x,, drop = FALSE]
    } else {
      p@site <- data.frame(p@site, df[x,, drop = FALSE])
    }
  
    p
  })
  
  SPC(profiles = lsp)
}