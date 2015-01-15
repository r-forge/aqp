# Accessors for SoilProfile and SPC classes
# 

## Get IDs
##
if (!isGeneric('ids'))
  setGeneric('ids', function(object) 
    standardGeneric('ids'))

setMethod("ids", "SoilProfile",
          function(object) {
            object@id
          }
)

setMethod("ids", "SPC",
          function(object) {
            laply(object@profiles, ids)
          }
)

## Get depths
##
if (!isGeneric('depths'))
  setGeneric('depths', function(object) 
    standardGeneric('depths'))

setMethod("depths", "SoilProfile",
          function(object) {
            object@depths
          }
)

setMethod("depths", "SPC",
          function(object) {
            lapply(object@profiles, depths)
          }
)

if (!isGeneric('depth_units'))
  setGeneric('depth_units', function(object) 
    standardGeneric('depth_units'))

setMethod("depth_units", "SoilProfile",
          function(object) {
            object@depth_units
          }
)

setMethod("depth_units", "SPC",
          function(object) {
            unique(laply(object@profiles, depth_units))
          }
)

## Get list or unique SoilProfile
##
if (!isGeneric('profiles'))
  setGeneric('profiles', function(object, i = NULL) 
    standardGeneric('profiles'))

setMethod("profiles", "SPC",
          function(object, i = NULL) {
            if (is.null(i)) object@profiles
            else object@profiles[[i]]
          }
)

## Get horizons data
##
if (!isGeneric('horizons'))
  setGeneric('horizons', function(object, ...) 
    standardGeneric('horizons'))

setMethod("horizons", "SoilProfile",
          function(object) {
            object@horizons
          })

setMethod("horizons", "SPC",
          function(object, as.list = FALSE) {
            res <- lapply(object@profiles, function(x) x@horizons)
            if (!as.list) {
              res <- data.frame(do.call('rbind', res), row.names = NULL)
            }
            res
          })

## Get site data
##
if (!isGeneric('site'))
  setGeneric('site', function(object, ...) 
    standardGeneric('site'))

setMethod("site", "SoilProfile",
          function(object) {
            object@site
          })

setMethod("site", "SPC",
          function(object, as.list = FALSE) {
            res <- lapply(profiles(object), site)
            if (!as.list) {
              res <- data.frame(do.call('rbind', res), row.names = NULL)
            }
            res
          }
)