
# convert munsell Hue, Value, Chroma into RGB
munsell2rgb <- function(the_hue, the_value, the_chroma)
	{
	# check for missing data
	if(missing(the_hue) | missing(the_chroma) | missing(the_value))
		stop('Must supply a valid Munsell color.')

	
	# check to make sure that each vector is the same length
	if(length(unique( c(length(the_hue),length(the_value),length(the_chroma)))) != 1)
		stop('All inputs must be vectors of equal length.')
	
	# load lookup table
	data(munsell)
	
	# perform subset
	s <- list()
	for(i in 1:length(the_hue))
		{
		s.i <- subset(munsell, 
		select=c('r','g','b'), 
		subset=hue == the_hue[i] & value == the_value[i] & chroma == the_chroma[i])
		
		# check for searches returning no matches
		if(nrow(s.i) == 0)
			s[[i]] <- data.frame(r=NA, g=NA, b=NA)
		else
			s[[i]] <- s.i
		}
	
	# return mathching RGB values as a data frame
	return(do.call('rbind', s))	
	}
	