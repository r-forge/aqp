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
"SoilProfile" <- function(depths=matrix(ncol=2), horizons=data.frame(), id=as.character(NA), depth_units="cm"){
  # if the first argument is a Profile object, it is used to construct the SoilProfile object
  if (!is(depths, "Profile")) {
    # creation of the Profile object 
    depths <- Profile(depths=depths, id=id, depth_units=depth_units)
  }
  # creation of the object (includes a validity check)
  new("SoilProfile", as(depths, "Profile"), horizons=horizons)
}