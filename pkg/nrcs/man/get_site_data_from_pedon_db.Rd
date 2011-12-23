\name{get_site_data_from_pedon_db}
\alias{get_site_data_from_pedon_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_site_data_from_pedon_db(dsn)
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
  q <- "SELECT site.usiteid as site_id, pedon.upedonid as pedon_id, 
  latdegrees + IIF(IsNull(latminutes), 0.0, latminutes/ 60.0) + IIF(IsNULL(latseconds), 0.0, latseconds / 60.0 / 60.0) as y,
  -(longdegrees + IIF(IsNull(longminutes), 0.0, longminutes / 60.0) + IIF(IsNull(longseconds), 0.0, longseconds / 60.0 / 60.0)) as x,
  dm.choice as datum,
  elev, slope, aspect, plantassocnm,
  bedrckdepth, br.choice_label as bedrock_kind,
  hs.choice AS hillslope_pos,
  descname as describer, soinmassamp as sampled_as, soinmascorr as correlated_as, psctopdepth, pscbotdepth,
  ps.choice_label as part_size_class
  FROM (
  (
  (
  (
  site INNER JOIN pedon ON site.siteiid = pedon.siteiidref)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1261) AS dm ON site.horizdatnm = dm.choice_id) 
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 517) AS br ON site.bedrckkind = br.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 127) AS ps ON pedon.taxpartsize = ps.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 971) AS hs ON site.hillslopeprof = hs.choice_id
  ORDER BY site.usiteid ;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

  # close connection
  odbcClose(channel)
  
  # warn if mixed datums
  if(length(unique(na.omit(d$datum))) > 1)
    warning('multiple datums present')
  
  # are there any dupes?
  if(any(table(d$pedon_id) > 1))
    warning('duplicate site/pedon information in results')
  
  # done
  return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
