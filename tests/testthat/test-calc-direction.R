# Test calc_direction
context('test calc_direction')

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
crs <- 32736
crs_lonlat <- 4326

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
get_geometry(DT, coords = coords, crs = crs)

DT[, dest_geometry := sf::st_centroid(sf::st_union(geometry))]
DT[, calc_direction(geometry, dest_geometry)]

lonlat_coords <- paste0('lonlat_', coords)
DT[, (lonlat_coords) := as.data.table(sf::st_coordinates(geometry))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry))]

# DT[, calc_direction(geometry_a = geometry, geometry_b = dest_geometry)]
# DT[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y,
#                     x_b = dest_X, y_b = dest_Y,
#                     crs = crs_lonlat)]

test_that('arguments provided correctly else error', {
  expect_error(
    DT[, calc_direction(x_a = X, geometry_a = geometry)]
  )

  expect_error(
    DT[, calc_direction(x_b = X, geometry_b = geometry)]
  )

  expect_error(
    DT[, calc_direction(y_a = X, geometry_a = geometry)]
  )

  expect_error(
    DT[, calc_direction(x_b = X, geometry_a = geometry)]
  )
})
# TODO: needs warning if combination are provided?. but cant/shouldnt if running by

# TODO: tests
# - is vect of units out
# - distances by st_distance and stats::dist are reasonably similar within tol
