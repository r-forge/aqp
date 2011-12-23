\name{get_colors_from_pedon_db}
\alias{get_colors_from_pedon_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_colors_from_pedon_db(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{
%%     ~~Describe \code{dsn} here~~
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
function(dsn)
  {
  require(RODBC)  
  require(plyr)
  require(Hmisc)
  
  # color data... check
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id, colormoistst, colorpct as pct, mh.choice AS colorhue, colorvalue, colorchroma
FROM (
(pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref)
INNER JOIN phcolor ON phorizon.phiid = phcolor.phiidref)
LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1242) AS mh ON phcolor.colorhue = mh.choice_id
ORDER BY pedon.upedonid, phiidref, colormoistst;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # convert Munsell to RGB
  cat('converting Munsell to RGB ...\n')
  d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))

  # re-combine
  d <- cbind(d, d.rgb)

  # split into dry / moist
  dry.colors <- subset(d, d$colormoistst == 1)
  moist.colors <- subset(d, d$colormoistst == 2)
  
  # mix and clean colors
  cat('mixing and cleaning colors ...\n')
  dry.colors.final <- ddply(dry.colors, c('pedon_id', 'hz_id'), mix_and_clean_colors, .progress='text')
  moist.colors.final <- ddply(moist.colors, c('pedon_id', 'hz_id'), mix_and_clean_colors, .progress='text')

  # rename columns
  names(dry.colors.final) <- c('pedon_id','hz_id','d_r','d_g','d_b')
  names(moist.colors.final) <- c('pedon_id','hz_id','m_r','m_g','m_b')

  # merge into single df
  d.final <- join(dry.colors.final, moist.colors.final, type='full')
  
  # clean-up
  rm(d, d.rgb, dry.colors, moist.colors, dry.colors.final, moist.colors.final)
  gc()
  
  # done
  return(d.final)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
