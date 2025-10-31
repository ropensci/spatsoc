# Test calc_direction
context('test calc_direction')

library(sf)

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

test_that('units are returned', {
  expect_s3_class(DT[, calc_direction(geometry)], 'units')
  expect_identical(DT[, units(calc_direction(geometry))$numerator], 'rad')
  expect_s3_class(
    DT[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
    'units'
  )
  expect_identical(
    DT[, units(
      calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)
    )$numerator],
    "rad"
  )
})

test_that('expected dims returned', {
  # N - 1 since missing start/dest for direction
  expect_length(DT[, calc_direction(geometry)], DT[, .N - 1])
  expect_length(
    DT[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
    DT[, .N - 1]
  )

  expect_length(DT[, calc_direction(geometry, geometry)], DT[, .N])
  expect_length(
    DT[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y,
                        x_b = lonlat_X, y_b = lonlat_Y, crs = crs_lonlat)],
    DT[, .N]
  )
})

test_that('expected range returned', {
  y <- units::set_units(pi, 'rad')
  expect_equal(DT[, min(calc_direction(geometry))], -y, tolerance = 0.1)
  expect_equal(DT[, max(calc_direction(geometry))], y, tolerance = 0.1)

  expect_equal(
    DT[, min(calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat))],
    -y,
    tolerance = 0.1
  )
  expect_equal(
    DT[, max(calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat))],
    y,
    tolerance = 0.1
  )
})

test_that('NAs returned as expected', {
  X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), lonlat_X := NA]
  expect_error(X_NA[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
               'missing values in coordinates')

  Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), lonlat_Y := NA]
  expect_error(Y_NA[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
               'missing values in coordinates')

  XY_NA <- copy(DT)[seq.int(100)][sample(.N, 10), (lonlat_coords) := NA]
  expect_error(XY_NA[, calc_direction(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
               'missing values in coordinates')

  get_geometry(XY_NA, lonlat_coords, crs_lonlat)
  expect_error(XY_NA[, calc_direction(geometry)],
               'missing values in coordinates')
  expect_error(XY_NA[, calc_direction(geometry, geometry_b = geometry)],
               'missing values in coordinates')
})
