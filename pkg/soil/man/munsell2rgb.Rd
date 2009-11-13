\name{munsell2rgb}
\Rdversion{1.1}
\alias{munsell2rgb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Munsell Notation to RGB
}
\description{
Color conversion based on a look-up table of common soil colors.
}
\usage{
munsell2rgb(the_hue, the_value, the_chroma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the_hue}{a vector of one or more more hues, upper-case}
  \item{the_value}{a vector of one or more values}
  \item{the_chroma}{a vector of one or more chromas}
}
\details{This function generalizes to vectorized usage, as long as the lenght of each argument is the same.}
\value{
A dataframe with columns 'r', 'g', and 'b'. If no match is found, NA is returned.
}
\references{
http://casoilresource.lawr.ucdavis.edu/drupal/node/201
}
\author{
Dylan E. Beaudette
}
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

# convert colors
cols.raw <- with(sp1, munsell2rgb(hue, value, chroma))
cols <- rgb(na.omit(cols.raw))

# simple plot, may need to tweak gamma-correction...
image(matrix(1:nrow(sp1)), axes=FALSE, col=cols, main='Soil Colors')

# convert into a more useful color space
require(colorspace)
sp1.lab <- as(with(sp1, RGB(r,g,b)), 'LAB')
plot(sp1.lab)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
