library(aqp)
library(latticeExtra)
library(plyr)
library(Hmisc)
library(reshape)


data(sp3)
depths(sp3) <- id ~ top + bottom

# how can we compute entropy from continuous variables?
# http://en.wikipedia.org/wiki/Differential_entropy
# http://cran.r-project.org/web/packages/entropy/

# calculation for continous random vars based on binning / counts
# http://cran.r-project.org/web/packages/entropy/entropy.pdf

## this isn't correct, and barfs when there is < 90% data available
f.entropy <- function(v) {
  # compute density, will use to map values -> probabilities
  # density has constraints on non-NA sample size
  d <- density(v, na.rm = TRUE, bw=1)
  # map values -> p via density estimation
  f <- splinefun(d)
  p <- f(v)
  # ... this doesn't sum to 1, density() is only approximate
  
  # shannon entropy is based on probabilities... which sum to 1
  h <- -sum(p * log(p, base=length(p)))
  
  # return fake lower / upper
  res <- c(value=h, lower=NA, upper=NA)
  return(res)
}

f.sig.to.noise <- function(v) {
  res <- mean(v, na.rm=TRUE) / sd(v, na.rm=TRUE)
    
  # return fake lower / upper
  res <- c(value=res, lower=NA, upper=NA)
  return(res)
}

# http://en.wikipedia.org/wiki/Quartile_coefficient_of_dispersion
f.qcd <- function(v) {
  res <- IQR(v, na.rm=TRUE) / median(v, na.rm=TRUE)
  
  # return fake lower / upper
  res <- c(value=res, lower=NA, upper=NA)
  return(res)
}

mean.and.sd <- function(values) {
  m <- mean(values, na.rm=TRUE)
  s <- sd(values, na.rm=TRUE)
  upper <- m + s
  lower <- m - s
  res <- c(value=m, lower=lower, upper=upper)
  return(res)
}


# compute some "information" metrics
a <- slab(sp3,  ~ clay + A + cec + ph, slab.fun=mean.and.sd, slab.structure=0:100)
a.1 <- slab(sp3,  ~ clay + A + cec + ph, slab.fun=f.entropy, slab.structure=0:100)
a.2 <- slab(sp3, ~ clay + A + cec + ph, slab.fun=f.sig.to.noise, slab.structure=0:100)
a.3 <- slab(sp3, ~ clay + A + cec + ph, slab.fun=f.qcd, slab.structure=0:100)


## TODO: WTF am I trying to accomplish here?
# # simulate a no-information comparison
# no.information.qcd <- function(i){
#   # "i" is a chunk of data.frame
#   x <- i$value
#   w <- i$contributing_fraction
#   
#   # remove NA
#   not.NA.idx <- which(!is.na(x))
#   x <- x[not.NA.idx]
#   w <- w[not.NA.idx]
#   
#   # remove O's
#   non.zero.idx <- which(x != 0)
#   x <- x[non.zero.idx]
#   w <- w[non.zero.idx]
#   
#   # re-sampled sum(QCD) from 10x original number of slices
#   n <- length(x)
#   s <- replicate(n, sample(x, size=n*10, prob = w, replace = TRUE))
#   base.qcd <- apply(s, 2, function(i) IQR(i) / median(i))
#   
#   d <- data.frame(value=base.qcd, lower=NA, upper=NA)
#   return(d)
# }
# 
# s <- slice(sp3, 0:100 ~ clay + A + cec + ph, just.the.data = TRUE)
# s.long <- melt(s, id.vars = c('top', 'bottom'), measure.vars = c('clay', 'A', 'cec', 'ph'))
# ddply(s.long, c('top', 'bottom', 'variable'), no.information.qcd)


# combine
g <- make.groups(summary=a, entropy=a.1, sig.to.noise=a.2, qcd=a.3)
g$which <- factor(g$which, labels=c('Mean +/- 1SD', 'psuedo-Entropy', 'Signal : Noise', 'QCD'))

p <- xyplot(
  top ~ value | which + variable, data=g,
  lower=g$lower, upper=g$upper, sync.colors=TRUE, alpha=0.5,
  cf=g$contributing_fraction,
  ylab='Depth (cm)',
  xlab='',
  ylim=c(100,-5), layout=c(5,3), scales=list(x=list(relation='free')),
  par.settings=list(superpose.line=list(lwd=2, col=c('RoyalBlue', 'Orange2'))),
  panel=panel.depth_function, 
  prepanel=prepanel.depth_function,
  auto.key=list(columns=2, lines=TRUE, points=FALSE)
)

useOuterStrips(p, strip=strip.custom(bg=grey(0.85)), strip.left = strip.custom(horizontal=FALSE, bg=grey(0.85)))


## investigate weighted mean QCD, weighted by contributing fraction
a.4 <- slab(sp3, ~ clay + A + cec + ph, slab.fun=f.qcd)

## must remove 0's and NA before computing weighted mean QCD
## when n=1, IQR = 0 => QCD = 0
##
## compare wt.mean QCD to empirical baseline
wtd.mean.qcd <- function(i){
  # "i" is a chunk of data.frame
  x <- i$value
  w <- i$contributing_fraction
  
  # remove NA
  not.NA.idx <- which(!is.na(x))
  x <- x[not.NA.idx]
  w <- w[not.NA.idx]
  
  # remove O's
  non.zero.idx <- which(x != 0)
  x <- x[non.zero.idx]
  w <- w[non.zero.idx]
  res <- wtd.mean(x, weights=w, na.rm=TRUE)
  
  # normalize to some kind of baseline
  n <- length(x)
  r <- sample(x, size=n * 100, prob = w, replace = TRUE)
  base.qcd <- IQR(r) / median(r)
  
  d <- data.frame(qcd=res, qcd.norm=res/base.qcd)
  return(d)
}

## weighted sum QCD and comparison to empirical baseline
wtd.sum.qcd <- function(i){
  # "i" is a chunk of data.frame
  x <- i$value
  w <- i$contributing_fraction
  
  # remove NA
  not.NA.idx <- which(!is.na(x))
  x <- x[not.NA.idx]
  w <- w[not.NA.idx]
  
  # remove O's
  non.zero.idx <- which(x != 0)
  x <- x[non.zero.idx]
  w <- w[non.zero.idx]
  
  # compute weighted sum
  res <- sum(x * w)
  
  # normalize to some kind of baseline,
  # re-sampled sum(QCD) from 10x original number of slices
  n <- length(x)
  s <- replicate(n, sample(x, size=n*10, prob = w, replace = TRUE))
  base.qcd <- sum(apply(s, 2, function(i) IQR(i) / median(i)))
  
  d <- data.frame(qcd=res, qcd.norm=res/base.qcd)
  return(d)
}

## how is this useful?
## TODO: compare between soils
ddply(a.4, 'variable', .fun=wtd.mean.qcd)
ddply(a.4, 'variable', .fun=wtd.sum.qcd)


