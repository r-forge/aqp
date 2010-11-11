data(sp1)

source('class_defs_S4.R')

# check simple case -- 1 profile
sp1.1 <- sp1[sp1$id == 'P001', ]
depths(sp1.1) <- id ~ top + bottom
site_data(sp1.1) <- ~ group

# check complex case -- multiple profiles
depths(sp1) <- id ~ top + bottom

sp1
