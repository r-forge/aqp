setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    ids="character",
    site="data.frame",
    profiles='list'
  ),
  prototype=prototype( 
    ids=as.character(NA),
    site=data.frame(),
    profiles=list(new("SoilProfile"))
  ),
  validity=function(object) {
    # number of ids and number of profiles must match
    if (length(object@ids) != length(object@profiles))
      stop("number of ids and number of profiles must match")
    # number of ids and number of sites must match
    if (length(object@ids) != nrow(object@site))
      stop("# number of ids and number of sites must match")

    return(TRUE)
  }
)