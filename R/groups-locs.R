libs <- c('data.table', 'sf')
lapply(libs, require, character.only = TRUE)


locs <- fread('input/caribou data 2009.csv')

x.field <- 'X_COORD'
y.field <- 'Y_COORD'
id.field <- 'COLLAR_ID'
buffer.width <- 50
crs <- CRS("+init=epsg:32614")


st_sf(locs[, .(get(x.field), get(y.field), get(id.field))])
st_sf

sp.pts <- SpatialPointsDataFrame(locs[, .(get(x.field), get(y.field))],
                                 proj4string = crs,
                                 data = locs[, .(id = get(id.field))])

plot(sp.pts)

buffers <- gBuffer(sp.pts, width=buffer.width, byid=TRUE)

plot(buffers)


plot(gBuffer(p1,width=-0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1)

pt.matrix <- locs[, matrix(c(get(x.field), get(y.field)), ncol = 2)]
id <- locs[, .(COLLAR_ID)]
coordinates(id) <- pt.matrix
# if crs
# crs(id)

plot(id)

buffers <- gBuffer(id, width=buffer.width, byid=TRUE)

plot(buffers)

un <- gUnaryUnion(buffers)
agg <- aggregate(buffers, un, mean)
dis <- disaggregate(agg)
ov.r <- over(dis, id, returnList=T)

sp.groups <- data.table(rowID = ov.r)[, spatialGroup := seq(rowID)]
grps.by.memb <- unnest(sp.groups, rowID)



### Packages ----
libs <- c('data.table', 'ggplot2', 'lubridate',
          'sp', 'rgeos', 'asnipe', 'adehabitatHR',
          'igraph')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- fread('input/caribou data 2009.csv')

### Set variables ----
# Buffer width in metres
buffer.width <- 50

### Functions ----
# TODO(alec): look into nabor package for return ids + nearest (see stkovrflw)
# Create spatial groups from input data.table returning
#   a data.table with spatial groups added. Uses gBuffer (units = m),
CalcSpatialGroups <- function(in.dt, x.field, y.field) {

  #   set coordinates of in dt to fields x y input
  coordinates(in.dt) <- c(x.field, y.field)
  #   TODO(alec): set options for in projs out projs
  proj4string(in.dt) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
  points <- spTransform(in.dt, CRS( '+init=epsg:32621' ))
  points@data$X_COOR <- points@coords[,1]
  points@data$Y_COOR <- points@coords[,2]
  buffers <- gBuffer(points, width=buffer.width, byid=TRUE)
  # TODO(alec): this requires better documentation..
  un <- gUnaryUnion(buffers)
  agg <- aggregate(buffers, un, mean)
  dis <- disaggregate(agg)
  ov.r <- over(dis, points, returnList=T)

  sp.groups <- data.table(rowID = ov.r)[, spatialGroup := seq(rowID)]
  grps.by.memb <- unnest(sp.groups, rowID)

  # ls.dts <- lapply(seq(ov.r), function(x){
  #   rbindlist(ov.r[x])[, spatialGroup := x]
  # })
  # dt <- rbindlist(ls.dts, use.names = F,
  #                 idcol = 'rowID')
}

### Pre-Processing ----
locs[ , julday := yday(FIX_DATE)]
locs[ , datetime := ymd_hms(paste(FIX_DATE, FIX_TIME))]
# TODO(alec): determine rounding appropriate. can be 5 mins, 2 hours, etc
locs[ , roundtime := round_date(datetime, unit = 'hours')]


# Built in function in data.table .GRP gives each group by a unique ID
locs[ , timeGroup := .GRP , by = roundtime]

# Create a unique ID for each row
locs[, rowID := rleid(datetime)]

### Spatial Processing ----

# warnings simply due to about mean applied to
#   some non numeric cols
groups <- locs[, CalcSpatialGroups(.SD, 'X_COORD', 'Y_COORD'),
               by = timeGroup]

groups[, uniqueGroup := .GRP , by = .(spatialGroup, timeGroup)]

### Network Processing ----
# Create group by individual matrix with asnipe
gbi <- get_group_by_individual(groups[, .(COLLAR_ID, uniqueGroup)],
                               data_format='individuals')

# Create network matrix with association weights
gbi.net <- get_network(gbi, data_format='GBI',
                       association_index='HWI')

# Convert to igraph object
gbi.grph <- graph.adjacency(gbi.net,mode='undirected',
                            diag=FALSE,
                            weighted=TRUE)
plot(gbi.grph)

### MCP & Centroids ----
# Create a single column dataframe with unique individual IDs
pts <- data.frame(groups[, COLLAR_ID])
# Set the coordinates of input data
coordinates(pts) <- groups[, .(X_COOR,Y_COOR)]

# Set projection system  of those coords
proj4string(pts) <- CRS('+init=epsg:32621')

min.cv.polys <- mcp(pts, 95)

ovr.mcps <- over(min.cv.polys, min.cv.polys, returnList = T)
ls.dts <- lapply(seq(ovr.mcps), function(x){
  rbindlist(ovr.mcps[x])[, mcpGroup := x]
})
mcp.overlay.dt <- rbindlist(ls.dts, use.names = F,
                            idcol = 'rowID')

#?QW: no chain rule here? But how does directionality work
#   in this case?

# Kernel home ranges instead of MCP?
# https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf
kernls <- kernelUD(pts, 'href')
plot(getverticeshr((kernls)))

# Centroid of points by collar id
mean.points <- groups[, .(mean(X_COOR), mean(Y_COOR)),
                      by=COLLAR_ID]

colnames(mean.points) <- c('COLLAR_ID', 'meanX', 'meanY')

### Plotting ----
# Igraph plots
# is_simple(gbi.grph) simplify(gbi.grph)
grp.layout <- layout.norm(
  as.matrix(mean.points[,2:3]))

(ig <- plot.igraph(gbi.grph, layout = grp.layout))
# use V() and E() to access vertex and edge attributes

# ?ggraph

#TODO(alec): separate this as a howto, move to implementations
#   if need be?


# ggplots
# Early season
ggplot(groups[julday > 140 & julday < 156]) +
  geom_point(aes(EASTING, NORTHING, color= as.factor(COLLAR_ID)))

# Late season
ggplot(groups[julday > 166]) +
  geom_point(aes(EASTING, NORTHING, color= as.factor(COLLAR_ID)))

# All days
ggplot(groups) +
  geom_point(aes(EASTING, NORTHING, color= as.factor(COLLAR_ID)))


# Points with centroids colored
ggplot(mean.points) +
  geom_polygon(aes(long,lat, fill = as.factor(id),
                   alpha = 0.2),
               data=fortify(min.cv.polys)) +
  geom_point(aes(X_COOR, Y_COOR,size = 2),
             data = groups) +
  geom_point(aes(meanX, meanY,
                 size=10, color = as.factor(COLLAR_ID)))

