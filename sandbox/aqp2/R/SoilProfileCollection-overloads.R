##
## overloads
##

## TODO: rbind
## TODO: overload unique() via digest eval of unique profiles
## TODO: SetReplaceMethod("$", SoilProfileCollection)
## TODO: subsetProfile
## TODO: SetMethod("[", SoilProfileCollection)

# Number of profiles in the collection
length.SoilProfileCollection <- function(x) length(profiles(x))

# Number of horizons
setMethod("nrow", "SoilProfile",
          function(x) {
            nrow(x@depths)
          }
)

setMethod("nrow", "SoilProfileCollection",
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

setMethod("min", "SoilProfileCollection",
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

setMethod("max", "SoilProfileCollection",
          function(x, na.rm = TRUE) {
            max(laply(profiles(x), max))
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
  signature = "SoilProfileCollection",
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

## matrix / DF style access: only to horizon data
##
## i = profile index
## j = horizon / slice index
##
setMethod("[", "SoilProfile",
          function(x, i, j, ...) {
            # In this case i is always one
            # Now subsetting horizon data using j
            
            if(!missing(j)) {
              
              j <- as.integer(j)
              
              if(any(is.na(j))) {
                stop('NA not permitted in horizon/slice index', call.=FALSE)
              }
              
            } else {
              j <- 1:nrow(x@horizons)
            }
            
            # Subset horizons in the profiles
            SoilProfile(id = x@id,
                  depths = x@depths[j,, drop = FALSE],
                  depth_units = depth_units(x),
                  sp = x@sp,
                  horizons = x@horizons[j,, drop = FALSE],
                  site = x@site
            )
          }
)

setMethod("[", "SoilProfileCollection",
        function(x, i, j, ...) {
            
          # check for missing i and j
          if(missing(i) & missing(j)) {
            stop('must provide either a profile index or horizon/slice index, or both', call.=FALSE)
          }
          
          # convert to integer
          if(!missing(i)) {
            
            if(any(is.na(i))) {
              stop('NA not permitted in profile index', call.=FALSE)
            }
              
            # convert logical to integer per standard vector/list
            # indexing rules (thanks Jos? Padarian for the suggestion!)
            if(is.logical(i)) {
              i <- (1:length(x))[i]
            }
            
            i <- as.integer(i)
            
          } else { # if no index is provided, the user wants all profiles
            i <- 1:length(x)
          }
          
          # Subset profiles 
          spc <- profiles(x)[i]
          
          # Now subsetting horizon data using j
          if(!missing(j)) {
            
            j <- as.integer(j)
            
            if(any(is.na(j))) {
              stop('NA not permitted in horizon/slice index', call.=FALSE)
            }
            
          }
          
          # Subset horizons in the profiles
          lspc <- lapply(spc, function(s) s[, j])
          
          SoilProfileCollection(profiles = lspc)
          
        }
      )

            
