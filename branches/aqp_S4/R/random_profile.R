#' Generates a random SoilProfile object
#'
#' @param ID a character or numeric id used for this profile
#' @param n_horizons a vector of possible number of horizons
#' @param min_thick minimum thickness criteria for a simulated horizon
#' @param max_thick maximum thickness criteria for a simulated horizon
#' @param n_prop number of simulated soil properties (columns in the returned dataframe)
#' @param units units in which depths are expressed (default to cm)
#'
#' @return A SoilProfile object
#'
#' @author Dylan Beaudette, Pierre Roudier (S4 port)
random_profile <- function(ID, n_horizons=3:6, min_thick=5, max_thick=30, n_prop=1, units='cm'){
  # sanity check
  if(missing(ID))
	  stop('must specify an ID')

  if(max_thick < min_thick)
	  stop('illogical horizon thickness constraints')

  # choose a number of horizons
  n_hz <- sample(n_horizons, 1)

  # generate hz top bnd
  tops <- as.numeric(c(0, aaply(1:(n_hz -1), 1, function(x) sample(min_thick:max_thick, 1))))
  bottoms <- as.numeric(c(tops[-1], sample(min_thick:max_thick, 1)))

  # combine into a df
  res <- data.frame(id=rep(ID, n_hz), top=cumsum(tops), bottom=cumsum(bottoms), name=paste('H', 1:n_hz,sep=''))

  # generate several properties
  # with different means / sd
  for(i in 1:n_prop)
    res <- data.frame(res, .generateProperty(n_hz=n_hz, i=i))

  depths(res) <- id ~ top + bottom
  res
}

.generateProperty <- function(n_hz, i, method='random'){
  p <- numeric(n_hz)
  if (method == 'random') {
    p[1] <- rnorm(1)
    for(j in 2:n_hz)
      p[j] <- p[j-1] + rnorm(1, mean=runif(n=1, min=-10, max=10), sd=runif(n=1, min=1, max=10))
  } else {
    # Would like to implement something using splines to force a certain shape 
    # along the profile.
    if (method == 'spline') {
     stop('this method has not been implemented yet.') 
    }
    else
      stop('wrong method selected.')
  }
  res <- as.data.frame(p)
  names(p) <- paste('p', i, sep='')
  res
}

#' Generates a random SoilProfileCollection object
#'
#' @param IDs a character or numeric vector of profile ids
#' @param n_horizons a vector of possible number of horizons
#' @param min_thick minimum thickness criteria for a simulated horizon
#' @param max_thick maximum thickness criteria for a simulated horizon
#' @param n_prop number of simulated soil properties (columns in the returned dataframe)
#' @param units units in which depths are expressed (default to cm)
#'
#' @return A SoilProfileCollection object
#'
#' @author Pierre Roudier
random_collection <- function(IDs, ...){
  SoilProfileCollection(profiles=llply(IDs, function(x) random_profile(ID=x, ...)))
}