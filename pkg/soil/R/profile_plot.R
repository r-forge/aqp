# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary


# generic function prototype
profile_plot <- function(x, ...) UseMethod("profile_plot")

# default method, i.e. for a dataframe
profile_plot.default <- function(top, bottom, name, max_depth, cols=NA, width=1, cex.names=0.5, ...)
	{
	
	if(missing(max_depth))
		{
		# get the range of depths
		max_depth <- max(bottom, na.rm=TRUE)
		}
		
	# start a new plot:
	par(mar=c(1,0,0,1))
	plot(0,0, type='n', xlim=c(0,2.5*width), ylim=c(max_depth+2, 0), axes=FALSE, ...)
	
	# add horizons
	rect(0, bottom, width, top, col=cols)
	
	# annotate with names
	mid <- (top+bottom)/2
	text(width, mid, name, pos=4, offset=0.1, cex=cex.names)
	}



# method for a SoilProfile class
# not finished
profile_plot.SoilProfile <- function(d, color='soil_color', width=1, cex.names=0.5, ...)
	{
	# start a new plot:
	par(mar=c(1,0,0,1))
	plot(0,0, type='n', xlim=c(0,2.5*width), ylim=c(d$max_depth+2, 0), axes=FALSE, ...)
	
	# add horizons
	rect(0, d$data$bottom, width, d$data$top, col=d$data[, color_col])
	
	# annotate with names
	mid <- with(d$data, (top+bottom)/2)
	text(width, mid, d$data$name, pos=4, offset=0.1, cex=cex.names)
	
	}
	
	
# method for SoilProfileList class
profile_plot.SoilProfileList <- function(d, color='soil_color', width=0.25, cex.names=0.5, ...)
	{
	# fudge factors
	extra_x_space <- 1
	extra_y_space <- 2
	
	# pre-compute nice range for depth axis, also used for plot init
	depth_axis_intervals <- pretty(seq(from=0, to=d$max_depth, by=10))
	
	# set margins... consider moving outside of function
	par(mar=c(0.5,0,0,1))
	
	# init plotting region
	plot(0, 0, type='n', xlim=c(1, d$num_profiles+extra_x_space), ylim=c(max(depth_axis_intervals), -2), axes=FALSE)
	
	# add horizons
	for(i in 1:d$num_profiles)
		{
		rect(i-width, d$data[[i]][,'bottom'], i + width, d$data[[i]][,'top'], col=d$data[[i]][, color])
	
		# annotate with names
		mid <- (d$data[[i]][, 'top'] + d$data[[i]][, 'bottom'])/2
		text(i + width, mid, d$data[[i]][,'name'], pos=4, offset=0.1, cex=cex.names)
		
		# ID
		text(i, -1, d$data[[i]]$id, pos=3, font=2, cex=cex.names)
		}
	
	# axis:
	depth_axis_labels <- paste(depth_axis_intervals, d$depth_units)
	axis(side=4, line=-2, las=2, at=depth_axis_intervals, labels=depth_axis_labels, cex.axis=cex.names)
		
 	# debugging:
 	# abline(v=1:d$num_profiles, lty=2)
	}
	
# profile_plot(sp1.list, color='soil_color')	
	
	