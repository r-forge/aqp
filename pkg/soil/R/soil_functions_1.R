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
profile_plot <- function(top, bottom, name, cols=NA, width=1)
	{
	
	# get the range of depths
	r <- range(c(top, bottom))
	
	# start a new plot:
	par(mar=c(0,2,0,1))
	plot(0,0, type='n', xlim=c(0,width+0.25), ylim=c(r[2]+2, 0), axes=FALSE, xlab='', ylab='')
	
	# add horizons
	rect(0, bottom, width, top, col=cols)
	
	# annotate with names
	text(width, d$top, d$name, adj=c(-1,1), cex=0.75)
	
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


##############################################################
## slotting functions ##
##############################################################


# convert a set of horizon depths and property into a continuous sequence
# returning a vector of standardized length
# suitable for slotting
unroll <- function(top, bottom, prop, max_depth, segment_size=NA, bottom_padding_value=NA)
	{
	
	# inverse RLE, to generate repeating sequence of property, n times
	p <- inverse.rle(list(lengths=bottom-top, values=prop))
	
	# total depth, in unit length
	p.len <- length(p)
	
	# number of NAs to prepend, in case our profile does not start at 0
	num.NA.prepend <- abs(0 - min(top))
	
	# number of NAs we need to append to match the deepest profile
	num.NA.append <- max_depth - (p.len + num.NA.prepend)
	
	# debug
	# print(paste(max_depth, num.NA.prepend, p.len, num.NA.append))
	
	# padd the result with NA: from the top down
	p.pad <- c(rep(NA, times=num.NA.prepend), p)
	
	# but only if the number of NA to append is positive
	if(sign(num.NA.append) == 1)
	p.pad <- c(p.pad, rep(bottom_padding_value, times=num.NA.append))
	
	# return vector, padded to max_depth
	return(as.vector(p.pad))
	}

#
# as of R 2.7 calling var() or anything that calls var()
# results in an error when there are not enough values
# previously NA was returned
#
conditional.sd <- function(x)
	{
	
	l <- length(na.omit(x))
	if(l >= 3)
		{
		x.sd <- sd(x, na.rm=TRUE)
		}
	else
		{
		x.sd <- NA	
		}
	
	return(x.sd)
	}
	
## this function will cause errors when the levels of an id (factor) do not match those levels that actually have data!!
## 
## this function will also break when horizon boundaries do not make sense
## 
# means and confidence intervals should be calculated by population defined by seg_size and n pedons
# note that this requires a wt column now
# 
#
# TODO: check depth probability calculations
# 
# TODO: when we don't care about weights, add median +/- 1.58 * IQR/sqrt(n)   for more robust summary
# 
# TODO: warnings generated in some cases... could it be when there is a single soil / group?
# 
soil.slot <- function(data, seg_size=NA, seg_vect=NA, return.raw=FALSE, use.wts=FALSE, compute.depth.prob=FALSE)
	{
	# what is the datatype of 'prop'
	prop.class <- class(data$prop)
	
	# print(paste('property is:', prop.class))
	
	# get the max depth for the entire dataset
	max_d <- max(data$bottom)
	
	# unroll the dataset, a pedon at a time
	x.unrolled <- by(data, data$id, function(i, m=max_d) unroll(top=i$top, bottom=i$bottom, prop=i$prop, max_depth=m))
	
	# note that these will be used later on, based on segmenting approach
	# reconstitute into a matrix with 1:n-depth interval rows, and n_pedons columns
	x.recon_original <- sapply(x.unrolled, '[')
	
	if(use.wts == TRUE)
		{
		# unroll a weight vector for each pedon
		x.unrolled.wts <- by(data, data$id, function(i, m=max_d) unroll(top=i$top, bottom=i$bottom, prop=i$wt, max_depth=m))
		
		# reconstitute weights:
		x.recon.wts_original <- sapply(x.unrolled.wts, '[')
		}
	
	# if we have a regular-interval segment size, re-group the data following the segmenting id
	if(!missing(seg_size) | !missing(seg_vect))
		{
		
		# use a user-defined segmenting vector, starting from 0		
		if(!missing(seg_vect))
			{
			wind.idx <- rep(seg_vect[-1], diff(seg_vect))[1:max_d]
			}
		# using a fixed-interval segmenting vector
		else
			{
			# generate a vector of unique segment ids
			# adding one extra can sometimes cause warnings... not sure if it matters
			segment_label <- 1:((max_d/seg_size)+1)
			
			# generate combined segment id vector
			# truncating at the max depth
			wind.idx <- rep(segment_label, each=seg_size)[1:max_d]
			}
		
		
		
		## warnings are being generated here
		## it looks like values are being recycled, possibly the weights
		# subset values and weights by id
		# note that we are lumping the subset values by id into a single row of a matrix
		x.recon <- try(do.call('rbind', by(x.recon_original, wind.idx, unlist) ))
		
		if(use.wts == TRUE)
			{
			# subset values by  id
			x.recon.wts <- try(do.call('rbind', by(x.recon.wts_original, wind.idx, unlist) ))
			}
			
		# use a user-defined segmenting vector, starting from 0		
		if(!missing(seg_vect))
			{
			
			# get actual length of segmented data
			# note that this might be less than the maximum depth suggested by the segmenting vector
			len <- nrow(x.recon)
			len.seg_vect <- length(seg_vect)
			
			# the actual max_depth may be less than the requested segments
			# in that case we will need to truncate the horizon label vector
			if(len < len.seg_vect)
				{
				seg_vect_legal_idx <- which( (seg_vect - max_d) <= 0)
				sv_clean <- seg_vect[c(1,seg_vect_legal_idx+1)]
				}
				
			# the actual depth may be more than the max depth requested in the seg_vect
			else if(len > len.seg_vect)
				{
				sv_clean <- c(seg_vect[-len.seg_vect], max_d)
				}
			
			# normal circumstances
			# ??? what are they?
			else
				{
				sv_clean <- seg_vect
				}
			
			len.clean <- length(sv_clean)
			
			# generate segment tops and bottoms
			# this generates an extra row sometimes, check for it below
			df.top_bottom <- data.frame(top=c(0,sv_clean[-c(1, len.clean)]), bottom=c(sv_clean[-c(1, len.clean)], max_d))
			
			# check for lower horizon where top == bottom, and remove it
			bad_hz_list_TF <- with(df.top_bottom, top == bottom)
			
			if(TRUE %in% bad_hz_list_TF)
				{
				bad_hz_list_idx <- which(bad_hz_list_TF)
				print(paste('Removing horizon with 0 thickness (', bad_hz_list_idx, ')', sep='' ))
				df.top_bottom <- df.top_bottom[-bad_hz_list_idx, ]
				}
			
			# return a dataframe with all values, indexed by segment
			# useful for looking at the distribution of properties by segment
			# bwplot(factor(seg_interval) ~ p, data=s)
			if(return.raw == TRUE)
					{
					# generate the index as an ordered factor, with labels in the depth order
					# note that we have to use the sv_clean vector, minus the first element which will always be 0
					s.idx <- factor(rep(sv_clean[-1], times=ncol(x.recon)), ordered=TRUE, levels=sv_clean[-1])
					return( data.frame(seg_interval=s.idx, p=as.vector(x.recon)) )
					}
			
			}
			
		# using a fixed-interval segmenting vector	
		else
			{
			# get the length of our segmented data set
			# and generate a new sequence of depths
			len <- nrow(x.recon)
			l.seq <- 1:len
			dz <- l.seq * seg_size
			
			# generate segment tops and bottoms
			df.top_bottom <- data.frame(top=c(0,dz[-length(dz)]), bottom=c(dz[-len], max_d))
			
			
			# return a dataframe with all values, indexed by segment
			# useful for looking at the distribution of properties by segment
			# bwplot(factor(seg_interval) ~ p, data=s)
			if(return.raw == TRUE)
					{
					# generate the index as an ordered factor, with labels in the depth order
					s.idx <- factor(rep(dz, times=ncol(x.recon)), ordered=TRUE, levels=dz)
					return( data.frame(seg_interval=s.idx, p=as.vector(x.recon)) )
					}

			}
				
		} # segmenting
	
	# no segmenting
	else
		{
		x.recon <- x.recon_original
		
		if(use.wts == TRUE)
			x.recon.wts <- x.recon.wts_original
		
		df.top_bottom <- data.frame(top=0:(max_d-1), bottom=1:max_d)
		}

	
	# compute row-wise summary statistics
	if(prop.class %in% c('numeric','integer'))
		{
		p.mean <- apply(x.recon, 1, mean, na.rm=TRUE)
		p.sd <- apply(x.recon, 1, conditional.sd)
		}
	
	# todo: update this to use a different column
	if(prop.class == 'character')
		{
		# the results of this operation are a list,
		# one element for each depth segment
		
		# get a vector of all possible categories		
		p.unique.classes <- as.vector(na.omit(unique(as.vector(x.recon))))
		
		# tabular frequences for complete set of possible categories
		p.table <- apply(x.recon, 1, function(i) { table(factor(i, levels=p.unique.classes), useNA='no')  } )
		
		
		# convert into a dataframe
		p.freq <- as.data.frame(t(p.table))
		
		# convert into proportions
		p.row.counts <- apply(p.freq, 1, sum)
		p.prop <- sweep(p.freq, 1, STATS=p.row.counts, FUN='/')
		
		## TODO: finish this
		# remove proportions of 0
# 		p.prop[which(p.prop <= 0.000001), ] <- NA
		
		}
		
	if(compute.depth.prob == TRUE)
		{
		p.prop <- apply(x.recon, 1, function(i) sum(i, na.rm=TRUE) / length(i))
		}
		
	# no way to use weights with character vectors yet...
	if(use.wts == TRUE)
		{
		## weighted mean calculation
		## reduces to standard mean, when weights are equal
		# compute the row-wise sum of weights vector
		wts_seg_sums <- apply(x.recon.wts, 1, sum, na.rm=TRUE) 
		
		# generate a row-wise fractional weight matrix,
		# same dimensions as reconstituted property matrix
		wt_matrix <- sweep(x.recon.wts, 1, STATS=wts_seg_sums, FUN='/')
		
		# scale each property by its associated weight
		x.recon.wted <- x.recon * wt_matrix
		
		# compute the mean by row, weights sum to 1, so there is no division step
		p.wtmean <- apply(x.recon.wted, 1, sum, na.rm=TRUE)
	
	
		## row-wise weighted sd calculations: only if there are 3 or more obs
		if(ncol(x.recon.wted) >= 3)
			{
			d1 <- apply(wt_matrix * x.recon^2, 1, sum, na.rm=TRUE)
			d2 <- apply(wt_matrix, 1, sum, na.rm=TRUE)
			d3 <- apply(x.recon * wt_matrix, 1, sum, na.rm=TRUE)^2
			
			n1 <- apply(wt_matrix, 1, sum, na.rm=TRUE)^2
			n2 <- apply(wt_matrix^2, 1, sum, na.rm=TRUE)
		
			# weighted variance
			var.wt <- (d1 * d2 - d3) / (n1 - n2)
			
			# weighted sd: id wt. variances less than 0 -- these were probably computed by too few observations
			var.wt[which(var.wt < 0)] <- NA
			p.wtsd <- sqrt(var.wt)
			}
		else # not enough obs to compute SD
			{
			p.wtsd <- NA
			}
			
		# re-make final dataframe, note that df.top_bottom is made based on segmenting/non-segmenting
		df.stats <- data.frame(p.mean, p.wtmean, p.sd, p.wtsd)
		}
	# no weithed stats needed
	else
		{
		if(prop.class %in% c('numeric','integer'))
			{
			df.stats <- data.frame(p.mean, p.sd)
			}
		if(prop.class == 'character')
			{
			df.stats <- data.frame(p.prop)
			}
		if(compute.depth.prob == TRUE)
			{
			df.stats <- data.frame(p.prop)
			}
		}
	
	
	## form into dataframe for returning to the user 
	# this is usually where we have problems, caused by bad horizon boundaries
	if(nrow(df.top_bottom) == nrow(df.stats))
		{
		x.slotted <- data.frame(df.top_bottom, df.stats)
		}
	# something is wrong
	else
		{
		print("ERROR!")
		print(data)
		}
		
	# done
	return(x.slotted)
	}





## dev stuff: used these to compute boot-strap mean and CI
## for some reason the CI did not match the means...

# 		# need at least three observations for CI
# 		p.conf.int <- apply(x.recon, 1, conditional_t.test, cl=cl)
# 		# re-format
# 		ci.trans <- t(p.conf.int)	
# 		ci.lower <- ci.trans[,1]
# 		ci.upper <- ci.trans[,2]


# 		ci.list <- tapply(p.mean.by_row, wind.idx, conditional_t.test, cl=cl)
# 		p.conf.int <- sapply(ci.list, '[')
# 		
# 		# re-format CI
# 		ci.trans <- t(p.conf.int)	
# 		ci.lower <- ci.trans[,1]
# 		ci.upper <- ci.trans[,2]

# use t.test to compute confidence intervals
# return c(NA,NA) if there are not enough data
conditional_t.test <- function(x, cl)
	{
	# strip atts
	x.vec <- as.vector(x)
	
	# how many real observations do we have:
	n <- length( na.omit(x.vec))
	
	# how many unique obs do we have?
	n.unique <- length( unique(na.omit(x.vec)))
	
	# can we compute confidence intervals?
	if(n >= 3 & n.unique >= 2)
		{
		ci <- t.test(x.vec, conf.level=cl)$conf.int
		}
	else
		{
		# need to return two NA's here, one for the upper and one for the lower CI
		ci <- c(NA,NA)
		}
	
	return(ci)
	}



boot_ci <- function(b, cl)
	{
	# strip atts
	x.vec <- as.vector(b$data)
	
	# how many real observations do we have:
	n <- length( na.omit(x.vec))
	
	# how many uniqu obs do we have?
	n.unique <- length( unique(na.omit(x.vec)))
	
	# can we compute confidence intervals?
	if(n >= 3 & n.unique >= 2)
		{
		ci <- boot.ci(b, type='basic', conf=cl)$basic[,4:5]
		}
	else
		{
		# need to return two NA's here, one for the upper and one for the lower CI
		ci <- c(NA,NA)
		}
	
	return(ci)
	}


boot_mean <- function(x)
	{
	# strip atts
	x.vec <- as.vector(x)
	
	b <- boot(na.omit(x.vec), weighted.mean, R=200)
	
	return(list(b=b, mean=b$t0))
	}


