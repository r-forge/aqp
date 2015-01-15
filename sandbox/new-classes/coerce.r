## Coercition methods

as.data.frame.SoilProfile <- function(x, ...)  {
  
  # ID (always defined)
  df_id <- data.frame(rep(x@id, times = nrow(x)))
  names(df_id) <- names(x@id)
  
  # Depths (always defined)
  df_depths <- data.frame(x@depths)
  
  # Spatial point
  if (.hasSpatialPoint(x)) {
    df_sp <- as.data.frame(coordinates(x@sp))
    # replicate coordinates for each hz
    df_sp <- ldply(1:nrow(x), function(x) df_sp)
  } else {
    df_sp <- NULL
  }
  
  # Horizon data
  if (nrow(horizons(x)) > 0) {
    df_hz <- horizons(x)
  } else {
    df_hz <- NULL
  }
  
  # Site data
  if (nrow(site(x)) > 0) {
    df_site <- site(x)
    # replicate site data for each hz
    df_site <- ldply(1:nrow(x), function(x) df_site)
  } else {
    df_site <- NULL
  }
  
  l_df <- list(df_id, df_depths, df_sp, df_hz, df_site)
  # Eliminate NULL fields
  l_df <- l_df[which(laply(l_df, function(x) !is.null(x)))]
  
  do.call('cbind', c(l_df, row.names = NULL))
}

setAs("SoilProfile", "data.frame", function(from)
  as.data.frame.SoilProfile(from))

as.data.frame.SPC <- function(x, ...) {
  data.frame( do.call('rbind', lapply(profiles(x), as.data.frame)) , row.names = NULL)
}

setAs("SPC", "data.frame", function(from)
  as.data.frame.SPC(from))

as.SoilProfileCollection.SPC <- function(from) {
  # Get col names of SPC slots
  nm <- list(
    id = unique(laply(profiles(from), function(x) names(x@id))),
    depths = colnames(do.call(rbind, depths(from))),
    site = names(site(from)),
    sp = colnames(coordinates(from)),
    p4s = proj4string(from)
  )
  
  .depths_aqp <- getMethod('depths<-', 'data.frame', "package:aqp")
  .site_aqp <- getMethod('site<-', 'SoilProfileCollection', "package:aqp")
  f_depths <- as.formula(paste(nm$id, "~", paste(nm$depths, collapse = "+")))
  spc <- .depths_aqp(as.data.frame(from), f_depths)
  
  # Initiate site slot if present
  if (length(nm$site) > 0) {
    f_site <- as.formula(paste("~", paste(nm$site, collapse = "+")))
    spc <- .site_aqp(spc, f_site)
  }
  
  # Initiate sp slot if present
  if (length(nm$sp) > 0) {
    .coordinates_aqp <- getMethod('coordinates<-', 'SoilProfileCollection', "package:aqp")
    .proj4string_aqp <- getMethod('proj4string<-', 'SoilProfileCollection', "package:aqp")
    
    f_sp <- as.formula(paste("~", paste(nm$sp, collapse = "+")))
    # Put coords in site data
    spc <- .site_aqp(spc, as.formula(paste("~", paste(nm$sp, collapse = "+"))))
    # Set coordinates and proj4string
    spc <- .coordinates_aqp(spc, f_sp)
    spc <- .proj4string_aqp(spc, proj4string(from))
  }
  
  spc
}

setAs("SPC", "SoilProfileCollection", function(from)
  as.SoilProfileCollection.SPC(from))
