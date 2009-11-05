\name{random_profile}
\alias{random_profile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
random_profile(id, n = c(3, 4, 5, 6), min_thick = 5, max_thick = 30, n_prop = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{ ~~Describe \code{id} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{min_thick}{ ~~Describe \code{min_thick} here~~ }
  \item{max_thick}{ ~~Describe \code{max_thick} here~~ }
  \item{n_prop}{ ~~Describe \code{n_prop} here~~ }
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

# generate 10 random profiles with default settings:
d.list <- lapply(1:10, function(i) random_profile(i))
d <- do.call('rbind', d.list)

# simple cluster analysis:
p <- profile_compare(d, vars=c('p1','p2','p3'), max_d=50, k=0)
plot(hclust(p))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
