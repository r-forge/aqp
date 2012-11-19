library(aqp)
library(plyr)
library(lattice)

# generate some fake soils data
d <- ldply(1:1000, random_profile)
depths(d) <- id ~ top + bottom

# create several groups of different sizes
d$g0 <- rep(1:100, each=10) # 10 / group
d$g1 <- rep(1:20, each=50) # 50 / group
d$g2 <- rep(1:10, each=100) # 100 / group
d$g3 <- rep(1:5, each=200) # 200 / group

# aggregate across varying group sizes
s <- system.time(slab(d, ~ p1 + p2 + p3))[3]
s0 <- system.time(slab(d, g0 ~ p1 + p2 + p3))[3]
s1 <- system.time(slab(d, g1 ~ p1 + p2 + p3))[3]
s2 <- system.time(slab(d, g2 ~ p1 + p2 + p3))[3]
s3 <- system.time(slab(d, g3 ~ p1 + p2 + p3))[3]

# combine results into data.frame
tt <- data.frame(profiles.per.group=factor(c(10, 50, 100, 200, 1000)), time=c(s0, s1, s2, s3, s))

# viz results
xyplot(time ~ profiles.per.group, data=tt, type='b', pch=15, cex=2, col='black')

