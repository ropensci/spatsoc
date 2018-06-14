##     GPS-data clustering software
##     Copyright (C) 2008 mike warren (mike@mike-warren.com)
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
##

### Packages ----
libs <- c('data.table', 'igraph', 'sp')
lapply(libs, require, character.only = TRUE)

### Import data ----
locs <- fread('input/mock-locs.csv')
locs <- locs[X_COORD != 0 & Y_COORD != 0]

x.col <- 'X_COORD'
y.col <- 'Y_COORD'
id.field <- 'ANIMAL_ID'
buffer.width <- 500
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

locs[, group := seq(1:4)]

proj.fields <- c('EASTING', 'NORTHING')
locs[, (proj.fields) := data.table::as.data.table(rgdal::project(cbind(get(x.col), get(y.col)),
                                                                 utm21N))]

locs[, rowID := .I]

## Time variables
locs[, idate := as.IDate(FIX_DATE)]
locs[, itime := as.ITime(FIX_TIME)]

locs[, year := year(idate)]
locs[, month := month(idate)]

locs[, jul := yday(idate)]

locs[, roundtime := lubridate::round_date(as.POSIXct(paste(idate, itime)), 'hour')]


##### !!! FOUR DAYS NOT HOURS

timeWindowFields <- c('lowTime', 'highTime')
locs[, datetime := as.POSIXct(paste(idate, itime))]
locs[, (timeWindowFields) := .(datetime - (4*3600), datetime + (4*3600))]

######
library(spatsoc)
bufferSize <- 300
locs[, spatialGroup := group_pts(.SD, bufferSize, crs = utm21N,
                      coordFields = c("EASTING", "NORTHING"),
                      idField = id.field)$spatialGroup]
locs

subcols <- locs[, .(lowTime, highTime, ANIMAL_ID, EASTING, NORTHING, spatialGroup)]

setkey(subcols, lowTime, highTime)

# subcols[, foverlaps(.SD, .SD, which = TRUE, nomatch = NA),
#         by = spatialGroup]

s <- split(subcols, by = 'spatialGroup')
l <- lapply(s, FUN = function(s){
  foverlaps(s, s)
})
t <- rbindlist(l, idcol = 'timegroup')

t[spatialGroup ==2]



b <- t[spatialGroup == 2 | spatialGroup == 3,
  group_pts(.SD, bufferSize, crs = utm21N,
                             coordFields = c("EASTING", "NORTHING"),
                             idField = id.field),
  by = .(timegroup, spatialGroup)]
t



range(as.numeric(names(l)))



rbindlist(lapply(seq_along(subcols$spatialGroup), FUN = function(sp){
  f <- foverlaps(subcols[spatialGroup == sp], subcols[spatialGroup == sp],
                 which = TRUE, nomatch = NA)
  g <- graph_from_edgelist(as.matrix(f))
  data.table(clusters(g)$membership)
}))

fx <- foverlaps(subcols, subcols, which = TRUE, nomatch = NA)
gx <- graph_from_edgelist(as.matrix(fx[!xid == yid]))
mx <- data.table(clusters(gx)$membership)




group_pts(.SD, bufferSize, crs = utm21N,
         coordFields = c("EASTING", "NORTHING"),
         idField = id.field)




as.matrix(f)
g <- graph_from_edgelist(as.matrix(f))

data.table(clusters(g)$membership)




locs[, .{f <- foverlaps(.SD, .SD, by = c())
         g <- graph_from_edgelist(as.matrix(f))
         clusters(g)$membership}]



nhours <- 4

# locs[, c('lowTime', 'highTime') := .(itime - (nhours * 3600), itime)]




locs[, max(datetime) - min(datetime)]

buffers <- rgeos::gBuffer(spPts, width = bufferWidth, byid = FALSE)
ovr <- sp::over(spPts, sp::disaggregate(buffers))
dt <- data.table::data.table(spPts@coords, id = spPts$id, spatialGroup = ovr)





# TODO(alec): This is a bottleneck..
GroupClustersInSpace <- function(dt){
  # Make a spatial points data frame with the ids
  sp.points <- SpatialPointsDataFrame(dt[, .(X,Y)],
                                      proj4string = CRS("+init=epsg:32614"),
                                      data = dt[, .(rowID)])

  # Buffer out the spatial points by the column buffer size
  bufs <- gBuffer(sp.points, dt[, Clus_rad_m],
                  byid = T, id = dt[, rowID])

  # Union (dissolve) all buffers to a single polygon feature
  un <- gUnaryUnion(bufs)

  # Disaggregate the non-touching buffers into individual features
  disag <- disaggregate(un)

  # Find membership to spatial groups over the buffers
  ov.r <- over(disag, sp.points, returnList=T)

  # # Return membership of spatial groups as data.table with list unnested to rows
  sp.groups <- data.table(members = ov.r)[, spatialGroup := seq(members)]
  unnest(sp.groups, members)
}





## OPTIONS you might want to change

# approximate target cluster radius, in meters (depends a little on
# the method selected below).
# CLUSTER_RADIUS = 300

# available clustering methods; see around line 200 for more
# explanation
# CLUSTER_METHODS = ['any', 'all', 'centroid']
# CLUSTER_METHOD = CLUSTER_METHODS[0]
# CLUSTER_METHOD == 'any': method "is new point within CLUSTER_RADIUS m of ANY point currently in the cluster"?
# CLUSTER_METHOD == 'all': method "is new point within CLUSTER_RADIUS m of ALL points currently in the cluster"?
# CLUSTER_METHOD == 'centroid':method "is new point within CLUSTER_RADIUS m of current centroid of cluster"?


# def greatCircleDistance(a,b):
# (haversine function)
#  return NA where no loc taken

# calc distance from self to other
# return null if self latlng, other latlng are NULL
# else return haversine dist

# calc time dif

## This represents a cluster, obviously

# class Cluster:
#   def __init__(self):
#   self.points = []
# self.centroidcache = None
#
# def clusterInCluster(self,c):
#   """returns True if the other cluster c is"inside" this one
# (i.e. if they can be merged); centroids must be within CLUSTER_RADIUS
# and the points must be within 4 days"""
#
# if the distance between a pair of centroids is less than the cluster radius
# check for time overlap within 4 days
#
# clusters are built by averaging the points
#
# check point inclusion in clusters
#
# calculate average distance from centroid
#
# calculate max distance from centroid
#
# calculate total time spread of cluster in hours
#
# calculate theoretical number of points which should be in cluster
# (max - min fix number)
#
# calculate number missing points (if theoretical is 10, missing 2, 4, 6 then missing = 3)
#
# count how many fixes were away from the cluster
#
# count how many 24 hour blocks starting from first loc in cluster (1130am - 1230pm next day = 2)
#
# check that clusters are equal
#
#
# read in file, with first column as header
#
#
# HIERARCHICAL CLUSTERING
# make each point a cluster containing just itself
# then merge all clusters until no more merging possible
#
# merge clusters within CLUSTER_RADIUS of each other
#
# double for loop. passing over where for(i in clusters) for j in clusters    i == j
#
# build empty cluster for i, add i points
# if j points are in i cluster, then add points
# else, add j points to j cluster
#
# # (he essentially does double the work...)
# checks for either original cluster a == i & original cluster b == j OR the reverse
# if they are equal, keep original clusters
# else update with i cluster of i and j points
