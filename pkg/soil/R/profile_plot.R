# generate a soil profile figure, from a generic dataframe
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary


# generic function prototype
profile_plot <- function(x, ...) UseMethod("profile_plot")

# default method
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
profile_plot.SoilProfile <- function(d, color_col, width=1, cex.names=0.5, ...)
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
# not finished: needs to be further generalized... geometry calcs are not standardized
profile_plot.SoilProfileList <- function(d, color_col, width=1, cex.names=0.5, ...)
	{
	
	par(mar=c(1,0,0,1))
	plot(0, 0, type='n', xlim=c(1,width*(d$num_profiles+1.5)), ylim=c(d$max_depth+2, -2), axes=FALSE, ...)
	
	# add horizons
	for(i in 1:d$num_profiles)
		{
		rect(i-(width/4), d$data[[i]][,'bottom'], i + (width/4), d$data[[i]][,'top'], col=d$data[[i]][, color_col])
	
		# annotate with names
		mid <- (d$data[[i]][, 'top'] + d$data[[i]][, 'bottom'])/2
		text(i + (width/4), mid, d$data[[i]][,'name'], pos=4, offset=0.1, cex=cex.names)
		
		# ID
		text(i, -1, d$data[[i]]$id, pos=3, font=2, cex=cex.names)
		}
	
	# axis:
	axis(side=4, line=-3, las=2, at=pretty(c(0,d$max_depth)), labels=paste(pretty(c(0,d$max_depth)), 'cm'), cex.axis=0.75)
	
	
	
# 	debugging:
# 	abline(v=1:d$num_profiles, lty=2)
	
	}
	
# profile_plot(sp1.list, color_col='soil_color')	
	
	