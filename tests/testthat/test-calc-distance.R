# Test calc_distance
context('test calc_distance')

library(sf)

DT <- fread('../testdata/DT.csv')
coords <- c('X', 'Y')
crs <- 32736
crs_longlat <- 4326

# Reduce test data size
DT <- DT[ID %in% c('A', 'B', 'C')]

get_geometry(DT, coords = coords, crs = crs)
get_geometry(DT, coords = coords, crs = crs, output_crs = 4326,
             geometry_colname = 'geometry_longlat')

DT[, dest_geometry := sf::st_centroid(sf::st_union(geometry))]
DT[, dest_geometry_longlat := sf::st_centroid(sf::st_union(geometry_longlat))]

coords_longlat <- paste0(coords, '_longlat')
DT[, (coords_longlat) := as.data.table(sf::st_coordinates(geometry_longlat))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry))]

dest_coords <- paste0('dest_', coords, '_longlat')
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry_longlat))]

test_that('arguments provided correctly else error', {
  expect_error(
    DT[, calc_distance(x_a = X, geometry_a = geometry)]
  )

  expect_error(
    DT[, calc_distance(x_b = X, geometry_b = geometry)]
  )

  expect_error(
    DT[, calc_distance(y_a = X, geometry_a = geometry)]
  )

  expect_error(
    DT[, calc_distance(x_b = X, geometry_a = geometry)]
  )
})

test_that('units are returned when use_dist is FALSE else numeric', {
  expect_s3_class(DT[, calc_distance(geometry, use_dist = FALSE)], 'units')
  expect_s3_class(DT[, calc_distance(geometry, st_shift_longitude(geometry),
                                     use_dist = FALSE)],
                  'units')
  expect_s3_class(
    DT[, calc_distance(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat, 
                       use_dist = FALSE)],
    'units'
  )

  expect_type(
    DT[, calc_distance(x_a = X, y_a = Y, crs = crs, use_dist = TRUE)],
    'double'
  )
  expect_type(
    DT[, calc_distance(x_a = X, y_a = Y, x_b = X + 100, y_b = Y + 100,
                       crs = crs, use_dist = TRUE)],
    'double'
  )
})

test_that('expected dims returned', {
  N <- 100
  expect_length(DT[seq.int(N), calc_distance(geometry, use_dist = FALSE)],
                N * N)
  expect_length(DT[seq.int(N), calc_distance(geometry, dest_geometry,
                                             use_dist = FALSE)],
                N)

  expect_length(
    DT[seq.int(N), calc_distance(x_a = X_longlat, y_a = Y_longlat, crs = crs_lonlat,
                                 use_dist = FALSE)],
    N * N
  )
  expect_length(
    DT[seq.int(N), calc_distance(x_a = X_longlat, y_a = Y_longlat,
                                 x_b = dest_X_longlat, y_b = dest_Y_longlat,
                                 crs = crs_longlat,
                                 use_dist = FALSE)],
    N
  )

  expect_length(DT[seq.int(N), calc_distance(geometry, use_dist = TRUE)],
                N * N)
  expect_length(DT[seq.int(N), calc_distance(geometry, geometry[1],
                                             use_dist = TRUE)],
                N)

  expect_length(
    DT[seq.int(N), calc_distance(x_a = X_longlat, y_a = Y_longlat, crs = crs_longlat,
                                 use_dist = TRUE)],
    N * N
  )
  expect_length(
    DT[seq.int(N), calc_distance(x_a = X_longlat, y_a = Y_longlat,
                                 x_b = dest_X_longlat, y_b = dest_Y_longlat,
                                 crs = crs_longlat,
                                 use_dist = TRUE)],
    N
  )
})

test_that('expected range returned', {
  expect_gte(DT[, min(calc_distance(geometry, use_dist = FALSE))],
                units::set_units(0, 'm'))
  expect_gte(DT[, min(calc_distance(x_a = X, y_a = Y, crs = crs,
                                    use_dist = TRUE))],
                0)
  expect_gte(DT[, min(calc_distance(geometry, geometry_b = dest_geometry,
                                    use_dist = FALSE))],
             units::set_units(0, 'm'))
  expect_gte(DT[, min(calc_distance(x_a = X_longlat, y_a = Y_longlat,
                                    x_b = dest_X_longlat, y_b = dest_Y_longlat,
                                    crs = crs_longlat
                                    use_dist = FALSE))],
             units::set_units(0, 'm'))
})

test_that('NAs returned as expected', {
  # NAs returned if use_dist = FALSE, in recent updates to sf
  # NAs are not returned if use_dist = TRUE, unless X and Y are both NA
  # This is handled in group_pts by subsetting where X and Y are not NA

  X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), X := NA]
  res <- X_NA[, calc_distance(x_a = X, y_a = Y, crs = crs, use_dist = TRUE)]
  expect_length(res, nrow(X_NA) * nrow(X_NA))
  expect_true(any(is.na(X_NA$X)))
  # expect_true(any(is.na(res)))

  Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), Y := NA]
  res <- Y_NA[, calc_distance(x_a = X, y_a = Y, crs = crs, use_dist = TRUE)]
  expect_length(res, nrow(Y_NA) * nrow(Y_NA))
  expect_true(any(is.na(Y_NA$Y)))
  # expect_true(any(is.na(res)))

  X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), X := NA]
  res <- X_NA[, calc_distance(x_a = X, y_a = Y, crs = crs, use_dist = FALSE)]
  expect_length(res, nrow(X_NA) * nrow(X_NA))
  expect_true(any(is.na(X_NA$X)))
  # expect_true(any(is.na(res)))

  Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), Y := NA]
  res <- Y_NA[, calc_distance(x_a = X, y_a = Y, crs = crs, use_dist = FALSE)]
  expect_length(res, nrow(Y_NA) * nrow(Y_NA))
  expect_true(any(is.na(Y_NA$Y)))
  # expect_true(any(is.na(res)))

  XY_NA <- copy(DT)[seq.int(100)][sample(.N, 10), (coords_longlat) := NA]
  res <- XY_NA[, calc_distance(x_a = X_longlat, y_a = Y_longlat, crs = 4326,
                               use_dist = FALSE)]
  expect_length(res, nrow(XY_NA) * nrow(XY_NA))
  expect_true(any(is.na(c(XY_NA$X_longlat, XY_NA$Y_longlat))))
  expect_true(any(is.na(res)))

  get_geometry(XY_NA, coords_longlat, crs_longlat)
  res <- XY_NA[, calc_distance(geometry, use_dist = FALSE)]
  expect_length(res, nrow(XY_NA) * nrow(XY_NA))
  expect_true(any(sf::st_is_empty(XY_NA$geometry)))
  # expect_true(any(is.na(res)))
})
