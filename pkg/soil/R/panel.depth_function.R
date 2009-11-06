
## needs some work for profile-aggregated data


panel.depth_function <- function(x, y, subscripts, groups, upper, lower, ...) {

# extract this panel's data
d <- data.frame(yhat=x, top=y, upper=upper[subscripts], lower=lower[subscripts], groups=groups[subscripts])

# levels in the groups, for color matching
ll <- levels(d$groups)

# add grid
panel.grid(h=-1, v=-1, lty=3, col=1)

# add conf. intervals
by(d, d$groups, function(d_i) {
# make conf.int polygon
panel.polygon(x=c(d_i$lower, rev(d_i$upper)), y=c(d_i$top, rev(d_i$top)), col=grey(0.7), border=NA, alpha=0.5)
})

# add main lines
by(d, d$groups, function(d_i) {
# lookup color
m <- match(unique(d_i$group), ll)
# add line
panel.lines(d_i$yhat, d_i$top, lwd=trellis.par.get('superpose.line')$lwd, col=trellis.par.get('superpose.line')$col[m])
})


}
