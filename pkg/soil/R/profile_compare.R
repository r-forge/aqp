
##############################################################
## profile classification functions ##
##############################################################


# Seems to scale to 1000 profiles with 5 variables, could use optimization
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
	
	# if the id column is not a factor, convert it to one:
	if(class(s$id) != 'factor')
		s$id <- factor(s$id)
	
	# identify the number of profiles
	n.profiles <- length(levels(s$id))
	
	# number of variables
	n.vars <- length(vars)
	
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
		# Gower's distance gives the best looking results, and automatically standardizes variables
		
		# compute the proportion of cases where NA occurs
		proportion_non_NA <- 1 - (length(which(is.na(sp))) / (n.profiles * n.vars))
		
		# use some criteria for deciding when to keep the dissimilarities for this slice
		if(proportion_non_NA >= 0.5)
			{
			d[[i]] <- daisy(sp, metric='gower')
			}
		
		# otherwise contribute no dissimilarity to the total
		else
			{
			print(paste(round(proportion_non_NA, 2), 'non-missing in slice', i))
			
			# generate an appropriately formatted dissimilarity matrix, full of NA
			m.ref <- lower.tri(matrix(ncol=n.profiles,nrow=n.profiles), diag=FALSE)
			m <- m.ref
			m[which(m.ref == FALSE)] <- NA
			
			# assign to this slice
			d[[i]] <- as.dist(m)
			
			# clean-up
			rm(m,m.ref)
			}
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