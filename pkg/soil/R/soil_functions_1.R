##############################################################
## spatial slicing functions ##
##############################################################


# format named depth slices into a list of SPDF objects
format_slices <- function(data, crs=NULL, depths, variable=NULL)
{	

	# check to make sure we have enough data to make an SPDF
	if(is.null(data$x) | is.null(data$y))
		stop('coordinates missing!')
	
	# init empty list
	slices <- list()
	
	for(i in depths)
	{
		# get this depth slice
		d.sub <- subset(data, subset=top==i)
		
		# convert to SPDF: 'x' and 'y' columns must be present
		coordinates(d.sub) <- ~ x+y	
		
		# this isn't quite right
		if(!is.null(crs))
			d.sub@proj4string <- crs
		
		# if a 'variable' is given, slice the data such that no NA
		# in that variable are returned
		if(!is.null(variable))
			{
			# keep only those points that have data
			no.na.idx <- which( ! is.na(d.sub@data[,variable]))
		
			# name each list component
			slices[[paste(i, 'cm', sep='-')]] <- d.sub[no.na.idx, ]
			}
		else # return all data regardless of NA in (un)named variable
			{
			slices[[paste(i, 'cm', sep='-')]] <- d.sub[]
			}	
	}
	return(slices)
}


# plot named slices of a given variable, cut into specified quantiles
plot_slices <- function(slices, region_outline, contours=NA, depths, variable, probs, legend.cex=1, symbol.cex=1.5)
	{
	# lookup quantiles of variable
	# for all slices
	global_quantiles <- quantile(do.call('c', lapply(slices, function(i) i@data[,variable])), na.rm=TRUE, probs)
	
	global_quantiles.names <- levels(cut(probs, breaks=probs))
	
	# setup plot regions: hard-coded for 3x2 layout
	m <- matrix(c(1,1,1,2,3,4,5,6,7,8,8,8), ncol=3, byrow=TRUE)
	layout(m, heights=c(0.1,1,1,0.2))
	
	# plot the main title
	par(mar=c(0,0,0,0))
	plot(1,1, axes=FALSE, type='n')
	text(1,1, variable, cex=1.5)
	
	# loop over slices
	for(i in 1:length(depths))
		{
		# title for each subfig
		title_i <- paste(depths[i], 'cm')
		
		# data cut into global quantiles
		v.quantiles <- cut(slices[[i]]@data[,variable], global_quantiles)
		
		# color scale
		cols <- rev(brewer.pal(n=length(global_quantiles)-1, name='Spectral'))
		
		
		# do the plot
		par(mar=c(1,1,2,1))
		plot(region_outline)
		if(!is.null(contours)) lines(contours, col=grey(0.9))
		points(slices[[i]], pch=21, cex=symbol.cex, col=1, bg=cols[as.numeric(v.quantiles)])
		title(title_i)
		box()
		}
	
	# legend
	ltext <- paste(levels(v.quantiles), global_quantiles.names, sep='\n')
	
	par(mar=c(0,0,0,0))
	plot(1,1, axes=FALSE, type='n')
	legend(1, 1, legend=ltext, col=1, pt.cex=2, pt.bg=cols, pch=21, horiz=TRUE, yjust=0.5, xjust=0.5, cex=legend.cex, bty='n')
	
	}



##############################################################
## data formatting functions ##
##############################################################

# 
# x.re.formatted <- by(x, x$pedon_id, function(d.sub) 
# {
# num_hz <- nrow(d.sub)
# last_hz <- d.sub[num_hz, ]
# last_hz$top <- last_hz$bottom 
# rbind(d.sub, last_hz)
# } 
# )
# 
# x.new <- do.call('rbind', x.re.formatted)


##############################################################
## plotting functions ##
##############################################################



# generate a soil profile figure
# using top and bottom boundaries, annotating with name
# optionally color with vector that is the same length as number of horizons
# behavior not defined for horizons with an indefinate lower boundary
profile_plot <- function(top, bottom, name, max_depth, cols=NA, width=1, cex.names=0.5, ...)
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



# 
# needs to be cleaned up
# 
depth_function_plot <- function(b, v, df=2, ...)
	{
	require(splines)
	# require(quantreg)
	
	# mid points
	d <- diff(b)/2 + b[-length(b)]
	
	
	# quantile regression
	# lm.sp <- rq(v ~ ns(d,df))
	
	# regular lm
	lm.sp <- lm(v ~ ns(d,df))
	
	new_depths <- seq(min(b), max(b))
	lm.sp.pred <- predict(lm.sp, data.frame(d=new_depths))
	
	plot(lm.sp.pred, new_depths, type='l', col='blue', xlim=c(0,max(v)+1), ylim=c(max(b)+1,0), ...)
	
	
	# plot the midpoints
	points(d ~ v)
	
	lines(approx(v,d), lty=2, col='red')
	
	# make the step function:
	segments(v, b[-length(b)], v, b[-1])
	segments(v[-length(v)], b[-c(1,length(b))], v[-1], b[-c(1,length(b))])
	}


# 
# this panel function is a little funky see usage notes:
# 
# x: the soil property vector
# y: a vector of depths, repeated twice to include top and bottom
# groups: grouping facor
# cols: a vector of colors, as long as the number of panels
# 
panel.soil_profile <- function(x, y, groups, subscripts, cols, ...)
	{
	
	make.segments <- function(df, lcol, ...)
		{
		
		n_hz <- length(df$prop) / 2
		
		# need at least 2 horizons
		if(n_hz > 1)
			{
			df.new <- data.frame(top=df$bnd[1:n_hz], bottom=df$bnd[(n_hz+1):length(df$prop)], prop=df$prop[1:n_hz])
			
			# print(df.new)
			
			# vertical segments
			panel.segments(df.new$prop, df.new$top, df.new$prop, df.new$bottom, col=lcol)  
			# horizontal segments
			panel.segments(df.new$prop[-n_hz], df.new$bottom[-n_hz], df.new$prop[-1], df.new$top[-1], col=lcol)
			}
			
		else
			{
			print(paste('only 1 horizon, skipping!', df$groups[1]))
			}
		
		}
	
	panel_color <- cols[panel.number()]
	
	# re-make a nice dataframe
	d <- data.frame(prop=x, bnd=y, groups=groups[subscripts])
	
	by(d, d$groups, make.segments, lcol=panel_color, ...)
	
	}

##############################################################
## profile classification functions ##
##############################################################



# TODO: convert soil_flag into a factor
# function requires at least two attributes
# hard coded reference to id
# seems to work with different total depths... need to check
# set k to 0 for no depth weighting 
profile_compare <- function(s, vars, max_d, k, replace_na=FALSE, add_soil_flag=FALSE, return_depth_distances=FALSE)
	{
	
	# check to make sure that there is an 'id' column
	if(is.null(s$id))
		stop("'s' must contain a column named 'id' ")
	
	# compute a weighting vector based on k	
	w <- 1 * exp(-k*1:max_d)
	
	
	# this approach requires a named list of soil properties
	s.unrolled <- by(s, s$id, function(di, p=vars, d=max_d) 
		{
		# init a temp list
		l <- list()
		# iterate over named properties:
		for(p_i in p)
			{
			# unroll each named property to matching component of our list
			# if the profiles are shallower than max_depth, padd with NA
			l[[p_i]] <- unroll(di$top, di$bottom, prop=di[,p_i], max_depth=d)
			}
			
		
		
		# add a soil flag as one of the attributes 
		# doesn't seem to help-- resulting groupings do not make sense
		if(add_soil_flag)
			{
			# generate a soil flag: 1=soil, 0=not soil
			max_soil_depth <- max(di$bottom)
			if(max_soil_depth >= d)
				remaining_non_soil <- 0
			else
				remaining_non_soil <- d - max_soil_depth
				
			l$soil_flag <- factor(c(rep(1, times=max_soil_depth), rep(0, times=remaining_non_soil )))
			}
			
		# convert list into z by p matrix
		m <- sapply(l, '[')
		}
	)
	
# 	return(s.unrolled)
	
	# init a list to store distance matrices, one for each depth interval
	d <- list()
	for(i in 1:max_d)
	{
		# for each z, generate distance matrix
		# note that we have to pass in variable 'i', as this is the 
		# current depth segment
		ps <- sapply(s.unrolled, function(dz, z_i=i) { dz[z_i,] })
		sp <- t(ps)
		
		# compute distance metric for this depth
		# distance metric has large effect on results
		# Gower's distance gives the best looking results...
		# figure out how to deal with soil_flag variable:
		# 		binary variable(s) 9 treated as interval scaled
		d[[i]] <- daisy(sp, metric='gower')
	}
	
	
	if(replace_na)
		{
		# replace all NA with the MAX distance between any observations
		max.distance <- max(sapply(d, max, na.rm=TRUE))
		d <- lapply(d, function(d_i) {d_i[which(is.na(d_i))] <- max.distance ; return(d_i)} )
		}
		
	# perform depth-weighting 	
	for(i in 1:max_d)
		d[[i]] <- d[[i]] * w[i]
	
	
	
	
	# optionally return the distances for each depth, after weighting
	if(return_depth_distances)
		return(d)
	
	
	# compute the total distance, for all dept intervals,
	# by pedon:
	# consider using mean diss, or something different that total
	d.vect <- apply(t(sapply(d, '[')), 2, sum, na.rm=TRUE)
	
	# identify the number of profiles
	n.profiles <- length(unique(as.numeric(s$id)))
	
	# now make into a combined distance matrix
	m.ref <- lower.tri(matrix(ncol=n.profiles,nrow=n.profiles), diag=FALSE)
	m <- m.ref
	m[which(m.ref == FALSE)] <- NA
	m[which(m.ref)] <- d.vect
	
	# coerce to 'dist' class
	D <- as.dist(m)
	
	# update labels from our list of hz-dissimilarities
	attr(D, 'Labels') <- attr(d[[1]], 'Labels')
	
	# add distance metric
	attr(D, 'Distance Metric') <- 'Gower'
	
	# return the distance matrix, class = 'dist'
	return(D)	
	}



