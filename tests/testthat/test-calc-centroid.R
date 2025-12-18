# Test calc_centroid
context('test calc_centroid')

library(sf)
sf_use_s2(TRUE)

DT <- fread('../testdata/DT.csv')
coords <- c('X', 'Y')
crs <- 32736
crs_longlat <- 4326

get_geometry(DT, coords = coords, crs = crs)
get_geometry(DT, coords = coords, crs = crs, output_crs = 4326,
             geometry_colname = 'geometry_longlat')

DT[, dest_geometry := st_centroid(st_union(geometry))]

coords_longlat <- paste0(coords, '_longlat')
DT[, (coords_longlat) := as.data.table(st_coordinates(geometry_longlat))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(st_coordinates(dest_geometry))]

test_that('arguments provided correctly else error', {
  expect_error(
    DT[, calc_centroid(x = X, geometry = geometry, use_mean = FALSE)],
    'arguments incorrectly provided'
  )

  expect_error(
    DT[, calc_centroid(y = Y, geometry = geometry, use_mean = TRUE)],
    'arguments incorrectly provided'
  )
})

test_that('data.table/numeric returned when coords provided, sf when geo', {
  expect_s3_class(DT[, calc_centroid(geometry)], 'sf')
  expect_s3_class(DT[, .(centroid = calc_centroid(geometry))][[1]], 'sfc')

  expect_s3_class(
    DT[, calc_centroid(x = X_longlat, y = Y_longlat, crs = crs_longlat)],
    'data.frame'
  )
  expect_s3_class(
    DT[, .(centroid = calc_centroid(x = X, y = Y, crs = crs))][1],
    'data.table'
  )

  expect_s3_class(
    DT[, calc_centroid(x = X, y = Y, crs = crs), by = ID],
    'data.table'
  )

  expect_type(
    DT[, calc_centroid(x = X, y = Y, crs = crs), by = ID]$X,
    'double'
  )
})

test_that('expected dims returned', {
  expect_length(DT[, calc_centroid(geometry)], 1L)
  expect_length(DT[, calc_centroid(x = X_longlat, y = Y_longlat, crs = crs_longlat)],
                2L)
  expect_equal(ncol(DT[, calc_centroid(x = X_longlat, y = Y_longlat, crs = crs_longlat)]),
               2L)
})

test_that('calc_centroid equals st_centroid for mean and length 1 inputs', {
  new_nms <- c('X', 'Y')

  i <- DT[, sample(.I, 1)]

  expect_equal(
    data.frame(setnames(DT[i, calc_centroid(x = X, y = Y, crs = crs, use_mean = TRUE)],
                        new = new_nms)),
    data.frame(DT[i, .(X, Y)])
  )

  expect_equal(
    data.frame(setnames(DT[i, calc_centroid(x = X, y = Y, crs = crs, use_mean = FALSE)],
                        new = new_nms)),
    data.frame(DT[i, .(X, Y)])
  )

  expect_equal(
    DT[i, calc_centroid(geometry = geometry, use_mean = FALSE)]$x,
    DT[i, sf::st_sf(geometry)]$geometry
  )

  expect_equal(
    DT[i, calc_centroid(geometry = geometry, use_mean = TRUE)]$x,
    DT[i, sf::st_sf(geometry)]$geometry
  )

  i_seq <- DT[, sample(.I, 100)]

  expect_equal(
    setnames(DT[i_seq, calc_centroid(x = X, y = Y, crs = crs, use_mean = TRUE)], new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i_seq, .(X, Y)], coords = seq.int(2), crs = crs)))
    ))
  )

  expect_equal(
    setnames(DT[i_seq, calc_centroid(x = X, y = Y, crs = NA_crs_, use_mean = TRUE)], new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i_seq, .(X, Y)], coords = seq.int(2), crs = NA_crs_)))
    ))
  )
})


test_that('crs_use_mean decides as expected', {
  expect_true(crs_use_mean(NA))
  expect_true(crs_use_mean(NULL))
  expect_true(crs_use_mean(32736))
  expect_false({sf::sf_use_s2(TRUE); crs_use_mean(4326)})
  expect_true(suppressWarnings({sf::sf_use_s2(FALSE); crs_use_mean(4326)}))
  expect_warning({sf::sf_use_s2(FALSE); crs_use_mean(4326)})
})


# These specific test results require recent commits to sf. Save for later.
# test_that('NAs returned as expected', {
#   X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), X := NA]
#   res <- X_NA[, calc_centroid(x = X, y = Y, crs = crs)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(st_coordinates(res))))
#
#   Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), Y := NA]
#   res <- Y_NA[, calc_centroid(x = X, y = Y, crs = crs)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(st_coordinates(res))))
#
#   XY_NA <- copy(DT)[seq.int(100)][sample(.N, 10), (coords_longlat) := NA]
#   res <- XY_NA[, calc_centroid(x = X_longlat, y = Y_longlat, crs = 4326)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(st_coordinates(res))))
#
#   get_geometry(XY_NA, coords_longlat, crs_longlat)
#   res <- XY_NA[, calc_centroid(geometry)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(st_coordinates(res))))
# })
