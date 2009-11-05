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

# hz top-boundaries
b <- c(0,3,12,26,60)

# mean hz values
v <- c(12,20,24,30)

profile_plot(b, v, xlab='% Clay', ylab='Depth (cm)', main='Example 1')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
