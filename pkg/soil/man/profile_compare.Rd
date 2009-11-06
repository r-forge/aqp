\name{profile_compare}
\alias{profile_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Numerical Soil Profile Comparison}
\description{
Performs a numerical comparison of soil profiles using named properties, based on a weighted, summed, depth-segment-aligned dissimilarity calculation.
}
\usage{
profile_compare(s, vars, max_d, k, replace_na = FALSE, 
add_soil_flag = FALSE, return_depth_distances = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{a dataframe with at least 2 columns of soil properties, and an 'id' column for each profile}
  \item{vars}{a vector with named properties that will be used in the comparison}
  \item{max_d}{depth-slices up to this depth are considered in the comparison}
  \item{k}{a depth-weighting coeficient, use '0' for no depth-weighting (see examples below)}
  \item{replace_na}{if TRUE, missing data are replaced by maximum dissimilarity (FALSE)}
  \item{add_soil_flag}{(experimental) addition of 'soil'/'non-soil' flag for use when comparing soil profiles with large differences in depth (FALSE)}
  \item{return_depth_distances}{return intermediate, depth-wise dissimilarity results (FALSE)}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
a dissimilarity matrix object of class 'dist'
}
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{
... based on the work of .... 
}
\seealso{\code{\link{unroll}}, \code{\link{soil.slot}}}
\examples{

## 1. check out the influence depth-weight coef:
require(lattice)
z <- rep(1:100,4)
k <- rep(c(0,0.1,0.05,0.01), each=100)
w <- 100*exp(-k*z)

xyplot(z ~ w, groups=k, ylim=c(105,-5), xlim=c(-5,105), type='l', 
ylab='Depth', xlab='Weighting Factor', panel=function(...) {panel.grid(h=-1,v=-1) ; panel.superpose(...)},
auto.key=list(columns=4, lines=TRUE, points=FALSE, title="k", cex=0.8, size=3)
)

## 2. basic implementation, requires at least two properties
data(sp1)
d <- profile_compare(sp1, vars=c('prop','group'), max_d=100, k=0.01)

# better plotting with ape package:
require(ape)
require(cluster)
h <- diana(d)
p <- as.phylo(as.hclust(h))
plot(ladderize(p), cex=0.75, label.offset=1, no.margin=TRUE)
tiplabels(col=cutree(h, 3), pch=15)

## 3. other uses of the dissimilarity matrix
require(MASS)
# Sammon Mapping
d.sam <- sammon(d)

# simple plot
plot(d.sam$points, type = "n", xlim=range(d.sam$points[,1] * 1.5))
text(d.sam$points, labels=row.names(d.sam$points), cex=0.5, col=cutree(h, 3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
