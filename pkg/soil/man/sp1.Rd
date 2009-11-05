\name{sp1}
\Rdversion{1.1}
\alias{sp1}
\docType{data}
\title{
Soil Profile Data Example 1
}
\description{
Soil profile data from Pinnacles National Monument, CA.
}
\usage{data(sp1)}
\format{
  A data frame with 59 observations on the following 6 variables.
  \describe{
    \item{\code{top}}{horizon upper boundary (inches)} 
    \item{\code{bottom}}{horizon lower boundary (inches)}
    \item{\code{name}}{horizon name}
    \item{\code{prop}}{field-estimated clay content of <2mm fraction}
    \item{\code{id}}{soil id}
    \item{\code{group}}{grouping label}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# prep environment
require(lattice)
require(soil)
data(sp1)

# slot all profiles into 1cm depth slices
s1 <- soil.slot(data=sp1)

# check
xyplot(top ~ p.mean + I(p.mean + p.sd) + I(p.mean - p.sd), 
data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2), 
panel=panel.superpose, ylim=c(110,-5))

# other segment sizes
# 5cm
s5 <- soil.slot(data=sp1, seg_size=5)

# 10cm segments:
s10 <- soil.slot(data=sp1, seg_size=10)

# 20cm
s20 <- soil.slot(data=sp1, seg_size=20)

# variation in segment-weighted mean property: very little
sapply(
list(s1,s5,s10,s20), 
function(i) with(i, sum((bottom - top) * p.mean) / sum(bottom - top)) 
)

# combined viz
g2 <- make.groups(s1, s5, s10, s20)

xyplot(cbind(top,bottom) ~ p.mean | which, groups=which, data=g2, 
ylim=c(120,-5), as.table=TRUE, 
panel=panel.soil_profile, scales=list(y=list(tick.number=10)),
cols=c(1,1,1,1), layout=c(4,1), 
xlab='Property', ylab='Depth (cm)', 
main='Soil Profile Averaging by Slotting')

}
\keyword{datasets}
