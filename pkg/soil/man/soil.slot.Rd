\name{soil.slot}
\Rdversion{1.1}
\alias{soil.slot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Soil Profile Slotting
}
\description{
Align soil properties to a user-defined basis.
}
\usage{
soil.slot(data, seg_size = NA, seg_vect = NA, return_raw = FALSE, use.wts = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{seg_size}{
%%     ~~Describe \code{seg_size} here~~
}
  \item{seg_vect}{
%%     ~~Describe \code{seg_vect} here~~
}
  \item{return_raw}{
%%     ~~Describe \code{return_raw} here~~
}
  \item{use.wts}{
%%     ~~Describe \code{use.wts} here~~
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
# setup environment
library(reshape)
library(lattice)
library(soil)

# load example data
data(sp1)

#
# 1. try slotting categorical variables
#

# normalize horizon names:
sp1$name[grep('O', sp1$name)] <- 'O'
sp1$name[grep('A[0-9]', sp1$name)] <- 'A'
sp1$name[grep('AB', sp1$name, ignore.case=TRUE)] <- 'A'
sp1$name[grep('BA', sp1$name)] <- 'B'
sp1$name[grep('Bt', sp1$name)] <- 'B'
sp1$name[grep('Bw', sp1$name)] <- 'B'
sp1$name[grep('C', sp1$name)] <- 'C'

# generate new data for testing soil.slot()
y <- with(sp1, data.frame(id=id, top=top, bottom=bottom, prop=name))
# convert name back to a character
y$prop <- as.character(y$prop)
# fix factor levels
y$id <- factor(y$id)

# default slotting-- 1cm intervals
a <- soil.slot(y)

# reshape into long format for plotting
a.long <- melt(a, id.var=c('top','bottom'))

# plot horizon type proportions
xyplot(top ~ value | variable, data=a.long, subset=value > 0,
ylim=c(150, -5), type='S', horizontal=TRUE, layout=c(4,1), col=1 )


#
# 2. depth probability
#
y <- with(sp1, data.frame(id=id, top=top, bottom=bottom, prop=1))
a <- soil.slot(y, compute.depth.prob=TRUE)
xyplot(top ~ p.prop, data=a, ylim=c(250, -5), type='S', horizontal=TRUE, asp=4)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
