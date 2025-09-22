# Test calc_centroid
context('test calc_centroid')

library(spatsoc)
library(data.table)
# DT <- fread('../testdata/DT.csv')
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)

source('R/get_sf.R')
DT_sf <- get_sf(DT, coords, projection)

source('R/calc_centroid.R')
library(sf)
DT_sf[, centroid := calc_centroid(geometry = geometry), by = group]
# Note: due to https://github.com/Rdatatable/data.table/issues/4415
#  need to recompute the bbox
DT_sf[, centroid := st_sfc(centroid, recompute_bbox = TRUE)]

plot(DT_sf$centroid)

DT_sf[, c('centroid_X', 'centroid_Y') := calc_centroid(x = X, y = Y),
      by = group]

DT_sf[group == 1]
DT_sf[group == 39]
