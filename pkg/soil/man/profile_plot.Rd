\name{profile_plot}
\alias{profile_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
profile_plot(b, v, df = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{v}{ ~~Describe \code{v} here~~ }
  \item{df}{ ~~Describe \code{df} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
library(soil)

#
# 1. single profile
# 

# fake data
tops <- c(0,3,12,26,60)
bottoms <-c(3,12,26,60,100)
# hz names
hz <- c('Oi','A','Bw','Bt','Cr')

# make plot
profile_plot(top=tops, bottom=bottoms, name=hz)

#
# 2. several profiles
#
require(plotrix)
data(sp1)

# assign color scale
sp1$color <- color.scale(sp1$prop, extremes=c('white','Royalblue'))

par(mfcol=c(1,9))
by(sp1, sp1$id, function(i) {
with(i, profile_plot(top, bottom, name, max_depth=250, col=color))
})


#
# 3. add to bottom of dendrogram
#
require(TeachingDemos)
require(ape)
require(cluster)

data(sp1)
d <- profile_compare(sp1, vars=c('prop','group'), max_d=100, k=0.01)

h <- diana(d)
p <- ladderize(as.phylo(as.hclust(h)))

par(mar=c(1,1,1,1))
p.plot <- plot(p, cex=0.5, label.offset=-3, direction='down', y.lim=c(200,0))
tiplabels(col=cutree(h, 3), pch=15)
edgelabels(1:16, cex=0.5)

# how can we place these?
# see package 'geiger'
# see which.edge()

# for first tip label
which.edge(p, 1) # ---> edge 8

subplot(profile_plot(top=tops, bottom=bottoms, name=hz, max_depth=200), 1.5, 80, size=c(0.75,3), type='fig', vadj=1)




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
