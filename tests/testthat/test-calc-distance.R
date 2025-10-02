# Test calc_distance
context('test calc_distance')

library(spatsoc)
library(data.table)
library(sf)

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
projection <- 32736

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)

DT_sf <- get_geometry(DT, coords = coords, crs = projection)

DT_sf[, centroid := calc_centroid(geometry = geometry), by = group]
# Note: due to https://github.com/Rdatatable/data.table/issues/4415
#  need to recompute the bbox
DT_sf[, centroid := st_sfc(centroid, recompute_bbox = TRUE)]

DT_sf[, c('centroid_X', 'centroid_Y') := calc_centroid(x = X, y = Y),
      by = group]

DT_sf[, distance := calc_distance(geometry_a = geometry, geometry_b = centroid)]
DT_sf[, distance_XY := calc_distance(x_a = X, y_a = Y, x_b = centroid_X, y_b = centroid_Y)]

DT_sf[1:10, calc_distance(geometry_a = geometry)]
DT_sf[1:10, calc_distance(x_a = X, y_a = Y)]

DT_sf[sample(.N, 10)]

# TODO: needs warning if combination are provided?. but cant/shouldnt if running by
DT_sf[1:10, calc_distance(geometry_a = geometry, x_a = X)]


# TODO: tests
# - is vect of units out
# - n NA out is eq n sf st is empty in
# - distances by st_distance and stats::dist are reasonably similar within tol

# Sequential not available directly:
# DT_sf[1:2, st_distance(geometry, geometry, by_element = TRUE)]
# from {amt}
# DT_sf[, sf::st_distance(head(geometry, -1), tail(geometry, -1), by_element = TRUE)]

# Matrix
DT_sf[1:2, st_distance(geometry, by_element = FALSE)]

# Accepts NAs if CRS matches
DT_sf[1, st_distance(geometry, st_sfc(st_point(), crs = st_crs(geometry)))]

# TODO: return units either approach?
# TODO: check classes input in calling function

