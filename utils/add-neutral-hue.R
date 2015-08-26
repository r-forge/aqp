library(aqp)
data(munsell)

# get a copy of the original table
m <- munsell

# add records for neutral hues
N <- expand.grid(hue='N', value=1:10, chroma=0, r=0.2, g=0.2, b=0.2)

# add at bottom
munsell <- rbind(m, N)

# save
save(munsell, file='munsell.rda')
