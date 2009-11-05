\name{unroll}
\alias{unroll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
unroll(top, bottom, prop, max_depth, segment_size = NA, bottom_padding_value = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{top}{ ~~Describe \code{top} here~~ }
  \item{bottom}{ ~~Describe \code{bottom} here~~ }
  \item{prop}{ ~~Describe \code{prop} here~~ }
  \item{max_depth}{ ~~Describe \code{max_depth} here~~ }
  \item{segment_size}{ ~~Describe \code{segment_size} here~~ }
  \item{bottom_padding_value}{ ~~Describe \code{bottom_padding_value} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(top, bottom, prop, max_depth, segment_size=NA, bottom_padding_value=NA)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
