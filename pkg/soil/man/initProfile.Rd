\name{initProfile}
\Rdversion{1.1}
\alias{initProfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialize SoilProfile Object
}
\description{
Initialize a SoilProfile object from a properly formatted dataframe.
}
\usage{
initProfile(d, depth_units = "cm", idcol = "id")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{a dataframe with minimum of 'id', 'top', 'bottom' columns}
  \item{depth_units}{linear unit of depth measurements}
  \item{idcol}{the name of the column to be used for uniquely identifying soil profiles}
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
\references{http://casoilresource.lawr.ucdavis.edu/}
\author{Dylan E. Beaudette}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(soil)
data(sp1)
sp1.1 <- initProfile(sp1[1:6, ])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
