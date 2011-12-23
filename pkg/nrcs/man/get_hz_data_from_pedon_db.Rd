\name{get_hz_data_from_pedon_db}
\alias{get_hz_data_from_pedon_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_hz_data_from_pedon_db(dsn)
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
  
  # this can be optimized
  # RF calculation should be done in  a sub-query
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id,
  phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, phfield, 
  IIF(IsNULL(f.total_frags_pct), 0, f.total_frags_pct) AS total_frags_pct
  FROM (
  (pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref) 
  LEFT OUTER JOIN (
    SELECT phiidref, SUM(fragvol) as total_frags_pct 
    FROM phfrags
    GROUP BY phiidref
    ) as f on phorizon.phiid = f.phiidref
  )
  ORDER BY pedon.upedonid, phorizon.hzdept ASC ;"
  
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
