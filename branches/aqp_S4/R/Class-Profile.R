setClass(
  Class="Profile", 
  representation=representation(
    depths='matrix', # 2-col matrix with top and bottom horizon depths
#     horizons='data.frame', # the data for each horizon
    id='character', # change it to profile_id ??
    depth_units='character'
  ), 
  prototype=prototype(
    depths=matrix(ncol=2),
#     horizons=data.frame(),
    id=as.character(NA),
    depth_units=as.character(NA)
  ),
  validity=function(object) {
#     # If there is horizon data available
#     if (length(object@horizons) > 0 ) {
#       # number of horizon and number of horizon data must match
#       if (nrow(object@horizons) != nrow(object@depths))
#         stop("number of horizons and number of samples along the profile do not match")
#     }

    # testing that the depths are passesd using a 2-col matrix
    if (ncol(object@depths) != 2)
      stop("unconsistent number of depths columns")
    # if the depths are not void (everything NA)
    if (!all(is.na(object@depths))) {
      # testing that the depths are consistently ordered
      if (any((object@depths[, 2] - object@depths[, 1]) < 0))
	stop("the depths matrix must be ordered with top depths first, and bottom depths second")
    }
     # Number of user_id must equals one (one per profile)
    if (length(object@id) != 1)
      stop("profile id must be unique")
    # units must be unique
    if (length(object@depth_units) != 1)
      stop("depths must be expressed using one unique unit.")

    return(TRUE)     
  }
)