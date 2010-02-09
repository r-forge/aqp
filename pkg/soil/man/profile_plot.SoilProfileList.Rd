\name{profile_plot.SoilProfileList}
\Rdversion{1.1}
\alias{profile_plot.SoilProfileList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
profile_plot.SoilProfileList(d, color = "soil_color", width = 0.25, cex.names = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{cex.names}{
%%     ~~Describe \code{cex.names} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(d, color='soil_color', width=0.25, cex.names=0.5, ...)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
