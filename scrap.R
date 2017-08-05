locs <- data.table::fread('input/mock-locs.csv')
locs <- locs[X_COORD != 0 & Y_COORD != 0]

x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ANIMAL_ID'
buffer.width <- 500
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

proj.fields <- c('EASTING', 'NORTHING')
locs[, (proj.fields) := data.table::as.data.table(rgdal::project(cbind(get(x.col), get(y.col)),
                                            utm21N))]

locs <- locs[NORTHING > 5360000 & EASTING < 650000]

l <- spatsoc::grp.lines(locs[, list(EASTING, NORTHING, ID = ANIMAL_ID)],
                   crs = utm21N)
l


lst <- data.table:::split.data.table(locs[, list(EASTING, NORTHING)],
                                     locs[, list(ANIMAL_ID)])

`%do%` <- foreach::`%do%`

# Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = sp::rbind.SpatialLines) %do% {
  mapview::coords2Lines(
    matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                        ID = id,
                        proj4string = sp::CRS(utm21N))
}
sp.lines

data.table::setorder(locs, FIX_DATE, FIX_TIME)
locs

in.dt <- locs[, list(EASTING, NORTHING, ID = ANIMAL_ID)]
lst <- data.table:::split.data.table(in.dt[, list(EASTING, NORTHING)],
                                     in.dt[, list(ID)])
#  ------------------------------------------------------------------------

as.matrix(c(lst[1]$EASTING, lst[1]$NORTHING))
lst[1][['EASTING']]

d <- lst[1]
d


`%do%` <- foreach::`%do%`
lst <- lst[1]
# Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
  mapview::coords2Lines(matrix(c(i[['EASTING']], i[['NORTHING']]), ncol = 2),
                        ID = id,
                        proj4string = sp::CRS(crs))
}
sp.lines

# #### grps lines
#
# # Split up the data.table by collar ID into lists
# lst <- data.table:::split.data.table(locs[, .(EASTING, NORTHING)], list(locs[[id.field]]))
#
# `%do%` <- foreach::`%do%`
#
# # Make each list of an individuals locs into a spatial lines [sp, mapview, foreach]
# sp.lines <- foreach::foreach(i = lst, id = names(lst), .combine = rbind) %do% {
# mapview::coords2Lines(as.matrix(i[, get(proj.fields[1]), get(proj.fields[2])]), ID = id,
#                proj4string = sp::CRS(utm21N))
# }
#
#
# # Buffer those lines by a preassigned buffer width
# buffers <- rgeos::gBuffer(sp.lines, width = buffer.width, byid = FALSE)
#
# # Find which buffers overlap each other and return as a list
# ovr <- sp::over(sp.lines, sp::disaggregate(buffers), returnList = T)
# dt <- data.table::data.table(id = names(ovr), group = ovr)
# dt
#

#
#
#
#
#
#
#
#
#
#
#
#
#
# #### grps pts
# # ##### this is not important for the package.... (prepping our own data)
# sp.pts <- sp::SpatialPointsDataFrame(locs[, ..proj.fields],
#                                  proj4string = sp::CRS(utm21N),
#                                  data = locs[, .(id = get(id.field))])
#
# #
# # spatsoc::grp.pts(sp.pts, 50)
# buffers <- rgeos::gBuffer(sp.pts, width = buffer.width, byid = TRUE)
#
#
# disbuf <- rbind(sp::disaggregate(buffers))
# # b <- SpatialPolygonsDataFrame(disbuf, data.frame(1:29))
#
# # ggplot(dt) + geom_point(aes(EASTING, NORTHING),
# #                         color = o) +
# #   guides(color = guide_colorbar()) +
# #   # polygon calls fortify, and the default x y are long lat with id for grouping
# #   geom_polygon(data = b, aes(long, lat, group = id, color = id), alpha = 0.2)
