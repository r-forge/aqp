# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary


# generic function prototype
profile_plot <- function(...)
  UseMethod("profile_plot")

# Tentative - Dylan to correct my dirty hacks in base graphics ;)
profile_plot.SoilProfile <- function(data, color='soil_color', width=0.2, name='name', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), plot.order=1:length(data), add=FALSE, scaling.factor=1, y.offset=0, max.depth=max(data), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, ...){

  # check for missing / bad soil color column
  # hack: just check the first object in the list
  if(! color %in% names(data))
    stop(paste('Invalid soil color column:', color))
  
  if(! name %in% names(data))
    stop(paste('Invalid horizon name column:', name))

  # fudge factors
  extra_x_space <- 1
  extra_y_space <- 2

  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)

  # set margins... consider moving outside of function
  par(mar=c(0.5,0,0,1))

  # init plotting region, unless we are appending to an existing plot
  if(!add)
    plot(0, 0, type='n', xlim=c(1, length(data) + extra_x_space), ylim=c(max(depth_axis_intervals), -4), axes=FALSE)

  # generate rectangle geometry
  y0 <- (depths(data)[, 2] * scaling.factor) + y.offset # bottom
  y1 <- (depths(data)[, 1] * scaling.factor) + y.offset # top
  
  # make rectangles (horizons)
  rect(1 - width, y0, 1 + width, y1, col=data[[color]])

  # annotate with names
  # first get the horizon mid-point
  mid <- ( y1 + y0 )/2
	  
  # optionally shrink the size of names if they are longer than a given thresh
  if(shrink) {
    names.to.shrink <- which(nchar(data[[name]]) > shrink.cutoff)
    cex.names.shrunk <- rep(cex.names, length(data))
    cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
    text(1 + width, mid, data[[name]], pos=4, offset=0.1, cex=cex.names.shrunk)
  }
  # standard printing of names, all at the same size
  else
    text(1 + width, mid, data[[name]], pos=4, offset=0.1, cex=cex.names)		
	
  # add the profile ID, optionally abbreviate
  if(abbr)
    text(1, y.offset, abbreviate(profile_id(data), abbr.cutoff), pos=3, font=2, cex=cex.id)
  # otherwise no abbreviations of the ID	
  else
    text(1, y.offset, profile_id(data), pos=3, font=2, cex=cex.id)

  # axis:
  depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
  depth_axis_labels <- paste(depth_axis_intervals, units(data))

  axis(side=4, line=-2.5, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
}

profile_plot.SoilProfileCollection <- function(data, color='soil_color', width=0.2, name='name', cex.names=0.5, cex.depth.axis=cex.names, cex.id=cex.names+(0.2*cex.names), plot.order=1:length(data), add=FALSE, scaling.factor=1, y.offset=0, max.depth=max(data), n.depth.ticks=5, shrink=FALSE, shrink.cutoff=3, abbr=FALSE, abbr.cutoff=5, ...){

  # check for missing / bad soil color column
  # hack: just check the first object in the list
  if(! color %in% names(data))
    stop(paste('Invalid soil color column:', color))
  
  if(! name %in% names(data))
    stop(paste('Invalid horizon name column:', name))

  # fudge factors
  extra_x_space <- 1
  extra_y_space <- 2

  # pre-compute nice range for depth axis, also used for plot init
  depth_axis_intervals <- pretty(seq(from=0, to=max.depth, by=1), n=n.depth.ticks)

  # set margins... consider moving outside of function
  par(mar=c(0.5,0,0,1))

  # init plotting region, unless we are appending to an existing plot
  if(!add)
    plot(0, 0, type='n', xlim=c(1, length(data) + extra_x_space), ylim=c(max(depth_axis_intervals), -4), axes=FALSE)

  # add horizons in specified order	
  for(i in 1:length(data)) {
    # convert linear sequence into plotting order
#     profileprofile_i_i <- plot.order[i]
    
    # generate rectangle geometry
    y0 <- (depths(profiles(data, i))[, 2] * scaling.factor) + y.offset # bottom
    y1 <- (depths(profiles(data, i))[, 1] * scaling.factor) + y.offset # top
  
    # make rectangles (horizons)
    rect(i - width, y0, i + width, y1, col=profiles(data, i)[[color]])

    # annotate with names
    # first get the horizon mid-point
    mid <- ( y1 + y0 )/2
	  
    # optionally shrink the size of names if they are longer than a given thresh
    if(shrink) {
      names.to.shrink <- which(nchar(profiles(data, i)[[name]]) > shrink.cutoff)
      cex.names.shrunk <- rep(cex.names, length(profiles(data, i)))
      cex.names.shrunk[names.to.shrink] <- cex.names.shrunk[names.to.shrink] * 0.8
      text(i + width, mid, profiles(data, i)[[name]], pos=4, offset=0.1, cex=cex.names.shrunk)
    }
    # standard printing of names, all at the same size
    else
      text(i + width, mid, profiles(data, i)[[name]], pos=4, offset=0.1, cex=cex.names)		
	  
    # add the profile ID, optionally abbreviate
    if(abbr)
      text(i, y.offset, abbreviate(profile_id(data)[i], abbr.cutoff), pos=3, font=2, cex=cex.id)
    # otherwise no abbreviations of the ID	
    else
      text(i, y.offset, profile_id(data)[i], pos=3, font=2, cex=cex.id)
  }

  # axis:
  depth_axis_tick_locations <- (depth_axis_intervals * scaling.factor) + y.offset
  depth_axis_labels <- paste(depth_axis_intervals, units(data))

  axis(side=4, line=-2.5, las=2, at=depth_axis_tick_locations, labels=depth_axis_labels, cex.axis=cex.depth.axis)
}