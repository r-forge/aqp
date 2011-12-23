\name{get_veg_from_AK_Site}
\alias{get_veg_from_AK_Site}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_veg_from_AK_Site(dsn)
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
  # basic query
  q <- "SELECT VegStop.usiteid as site_id, localplant.lplantsym as plantsym, localplant.lplantname as plantname, vegetation.COVER as pct_cover
  FROM 
  (VegStop INNER JOIN vegetation ON VegStop.vegstopdbid = vegetation.vegstodbiidref)
  INNER JOIN localplant ON vegetation.lplantsym = localplant.lplantuid
  ORDER BY VegStop.usiteid ASC;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # done
  return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
