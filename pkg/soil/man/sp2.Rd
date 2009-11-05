\name{sp2}
\Rdversion{1.1}
\alias{sp2}
\docType{data}
\title{
Soil Profile Data Example 2
}
\description{
Soil horizonation and clay content for 617 soil profile from the Soil-Veg project.
}
\usage{data(sp2)}
\format{
  A data frame with 2614 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{soil id}
    \item{\code{top}}{horizon upper boundary (inches)} 
    \item{\code{bottom}}{horizon lower boundary (inches)}
    \item{\code{prop}}{clay content of the <2mm fraction, measured by hydrometer}
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
data(sp2)


# slot all profiles into 1cm depth slices
s1 <- soil.slot(data=sp2)

# check
xyplot(top ~ p.mean + I(p.mean + p.sd) + I(p.mean - p.sd), 
data=s1, type='S', horizontal=TRUE, col=1, lty=c(1,2,2), 
panel=panel.superpose, ylim=c(110,-5))

# other segment sizes
# 5cm
s5 <- soil.slot(data=sp2, seg_size=5)

# 10cm segments:
s10 <- soil.slot(data=sp2, seg_size=10)

# 20cm
s20 <- soil.slot(data=sp2, seg_size=20)

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
