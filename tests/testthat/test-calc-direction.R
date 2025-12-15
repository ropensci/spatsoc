# Test calc_direction
context('test calc_direction')

library(sf)
sf_use_s2(TRUE)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
coords <- c('X', 'Y')
crs <- 32736
crs_longlat <- 4326

get_geometry(DT, coords = coords, crs = crs)
get_geometry(DT, coords = coords, crs = crs, output_crs = 4326,
             geometry_colname = 'geometry_longlat')

DT[, dest_geometry_longlat := sf::st_centroid(sf::st_union(geometry_longlat))]

coords_longlat <- paste0(coords, '_longlat')
DT[, (coords_longlat) := as.data.table(sf::st_coordinates(geometry_longlat))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry_longlat))]

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
  expect_s3_class(DT[, calc_direction(geometry_longlat, use_transform = FALSE)],
                  'units')
  expect_identical(DT[, units(
    calc_direction(geometry_longlat, use_transform = FALSE))$numerator],
    'rad'
  )

  expect_s3_class(
    DT[, calc_direction(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat,
                        use_transform = FALSE)],
    'units'
  )
  expect_identical(
    DT[, units(
      calc_direction(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat,
                     use_transform = FALSE)
    )$numerator],
    "rad"
  )
})

test_that('expected dims returned', {
  # N - 1 since missing start/dest for direction
  expect_length(DT[, calc_direction(geometry_longlat, use_transform = FALSE)],
                DT[, .N - 1])
  expect_length(
    DT[, calc_direction(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat,
                        use_transform = FALSE)],
    DT[, .N - 1]
  )

  expect_length(DT[, calc_direction(geometry_longlat, geometry_longlat,
                                    use_transform = FALSE)], DT[, .N])
  expect_length(
    DT[, calc_direction(x_a = X_longlat, y_a = Y_longlat,
                        x_b = X_longlat, y_b = Y_longlat, crs = crs_longlat,
                        use_transform = FALSE)],
    DT[, .N]
  )
})

test_that('expected range returned', {
  y <- units::set_units(pi, 'rad')
  expect_equal(DT[, min(calc_direction(geometry_longlat, use_transform = FALSE))], -y, tolerance = 0.1)
  expect_equal(DT[, max(calc_direction(geometry_longlat, use_transform = FALSE))], y, tolerance = 0.1)

  expect_equal(
    DT[, min(calc_direction(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat, use_transform = FALSE))],
    -y,
    tolerance = 0.1
  )
  expect_equal(
    DT[, max(calc_direction(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat, use_transform = FALSE))],
    y,
    tolerance = 0.1
  )
})

test_that('NAs returned as expected', {
  X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), X_longlat := NA]
  expect_error(X_NA[, calc_direction(x_a = X_longlat, y_a = Y_longlat,
                                     crs = crs_longlat,
                                     use_transform = FALSE)],
               'missing values in coordinates')

  Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), Y_longlat := NA]
  expect_error(Y_NA[, calc_direction(x_a = X_longlat, y_a = Y_longlat,
                                     crs = crs_longlat,
                                     use_transform = FALSE)],
               'missing values in coordinates')

  XY_NA <- copy(DT)[seq.int(100)][sample(.N, 10), (coords_longlat) := NA]
  expect_error(XY_NA[, calc_direction(x_a = X_longlat, y_a = Y_longlat,
                                      crs = crs_longlat,
                                      use_transform = FALSE)],
               'missing values in coordinates')

  get_geometry(XY_NA, coords_longlat, crs_longlat)
  expect_error(XY_NA[, calc_direction(geometry,
                                      use_transform = FALSE)],
               'missing values in coordinates')
  expect_error(XY_NA[, calc_direction(geometry, geometry_b = geometry,
                                      use_transform = FALSE)],
               'missing values in coordinates')

  geometry_NA <- copy(DT)[seq.int(100)][
    sample(.N, 10), geometry_longlat := st_sfc(st_point())]
  expect_error(geometry_NA[, calc_direction(geometry_longlat)],
               'missing values in coordinates')
  expect_error(geometry_NA[, calc_direction(geometry_longlat, geometry_longlat)],
               'missing values in coordinates')
})
