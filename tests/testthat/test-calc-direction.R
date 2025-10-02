# Test calc_direction
context('test calc_direction')

library(spatsoc)
library(data.table)
library(sf)
library(units)

# TODO: use test data later
# DT <- fread('../testdata/DT.csv')
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
crs <- 32736


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)

get_geometry(DT, coords = coords, crs = crs)

source('R/calc_centroid.R')
source('R/calc_direction.R')
library(sf)
DT[, centroid := calc_centroid(geometry = geometry), by = group]
# Note: due to https://github.com/Rdatatable/data.table/issues/4415
#  need to recompute the bbox
DT[, centroid := st_sfc(centroid, recompute_bbox = TRUE)]

DT[, c('centroid_X', 'centroid_Y') := calc_centroid(x = X, y = Y),
      by = group]

# Note: due to st_geod_azimuth not accepting null geometries
DT[!sf::st_is_empty(geometry) &
        !sf::st_is_empty(centroid),
      direction := calc_direction(geometry_a = geometry, geometry_b = centroid)]
DT[, direction_XY := calc_direction(x_a = X, y_a = Y, x_b = centroid_X, y_b = centroid_Y)]

DT[!sf::st_is_empty(geometry),
      direction_self := c(calc_direction(geometry_a = geometry),
                          units::as_units(NA, 'rad')),
      by = ID]
DT[, direction_XY_self := c(calc_direction(x_a = X, y_a = Y),
                               NA),
      by = ID]

DT[sample(.N, 10)]

# TODO: needs warning if combination are provided?. but cant/shouldnt if running by

# TODO: tests
# - is vect of units out
# - distances by st_distance and stats::dist are reasonably similar within tol
