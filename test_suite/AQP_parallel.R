## works as of 2011-01-26
## DEB

library(aqp)
require(doMC)

set.seed(1010)
d <- ldply(1:100, function(i) random_profile(i))

##
## soil.slot.multiple()
##

# serial version:
#  user.self   sys.self    elapsed user.child  sys.child 
#    23.3599     0.0772    23.4771     0.0000     0.0000 
options(AQP_parallel=FALSE)
serial.times <- replicate(10, system.time(a <- soil.slot.multiple(d, g='id', vars=c('p1','p2', 'p3', 'p4'), seg_size=5)))


# parallel version: 
#  user.self   sys.self    elapsed user.child  sys.child 
#     0.8809     0.0664    12.8942    23.0982     0.4864
options(AQP_parallel=TRUE)
parallel.times <- replicate(10, system.time(a.p <- soil.slot.multiple(d, g='id', vars=c('p1','p2', 'p3', 'p4'), seg_size=5)))



##
## profile_compare() -- biggest speed improvement from converting for-loop to llply
## 

# serial version:
# using a for-loop:
#    user  system elapsed 
#   9.244   0.140   9.452
# 
# using llply:
#  user.self   sys.self    elapsed user.child  sys.child 
#     2.8422     0.0740     2.9894     0.0000     0.0000 


options(AQP_parallel=FALSE)
# source('profile_compare.R')
serial.times <- replicate(10, system.time(p <- profile_compare(d, vars=c('p1','p2', 'p3', 'p4'), max_d=100, k=0)))


#  user.self   sys.self    elapsed user.child  sys.child 
#     1.8361     0.0856     2.8351     1.3845     0.2020
options(AQP_parallel=TRUE)
parallel.times <- replicate(10, system.time(p.parallel <- profile_compare(d, vars=c('p1','p2', 'p3', 'p4'), max_d=100, k=0)))













d <- data.frame(y=rnorm(1000), id=rep(letters[1:4], each=500))

f <- function(x) {
m <- vector(length=10000)
for(i in 1:10000) {
	m[i] <- mean(sample(x$y, 100))
	}
	
mean(m)
}

library(plyr)
library(doMC)
registerDoMC(cores=2)

system.time(ddply(d, .(id), .fun=f, .parallel=FALSE))
system.time(ddply(d, .(id), .fun=f, .parallel=TRUE))

