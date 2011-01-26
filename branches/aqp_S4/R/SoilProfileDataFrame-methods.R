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
"SoilProfileDataFrame" <- function(depths=matrix(ncol=2), id=as.character(NA), depth_units="cm", horizons=data.frame(), site=data.frame()){
  # if the first argument is NOT a *Profile object, we build the object from scratch
  if (!inherits(depths, "Profile")) {
    depths <- Profile(depths,  id=id, depth_units=depth_units)
    depths <- SoilProfile(depths, horizons=horizons)
  }
  # otherwise we use that object to construct the object
  else {
    # if it is a profile object we have to construct a SoilProfile object first
    if (class(depths) == "Profile") {
      depths <- SoilProfile(depths, horizons=horizons)
    }
    # if it is already a SoilProfileDF object, we erase its site information
    if (class(depths) == "SoilProfileDataFrame") {
      depths <- as(depths, "SoilProfile")
    }
  }
  
  # creation of the object (includes a validity check)
  new("SoilProfileDataFrame", depths, site=site)
}

## accessors

if (!isGeneric("site"))
  setGeneric("site", function(object, ...)
    standardGeneric("site"))

setMethod("site", "SoilProfileDataFrame",
#' Retrieves the horizon information within a profile
#'
  function(object)
    object@site
)