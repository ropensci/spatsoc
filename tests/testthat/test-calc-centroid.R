# Test calc_centroid
context('test calc_centroid')

DT <- fread('../testdata/DT.csv')
coords <- c('X', 'Y')
crs <- 32736
crs_lonlat <- 4326

get_geometry(DT, coords = coords, crs = crs)

DT[, dest_geometry := sf::st_centroid(sf::st_union(geometry))]

lonlat_coords <- paste0('lonlat_', coords)
DT[, (lonlat_coords) := as.data.table(sf::st_coordinates(geometry))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry))]

# DT_sf[, centroid := calc_centroid(geometry = geometry), by = group]
# Note: due to https://github.com/Rdatatable/data.table/issues/4415
#  need to recompute the bbox
# DT_sf[, centroid := st_sfc(centroid, recompute_bbox = TRUE)]

# DT_sf[, centroid := calc_centroid(x = X, y = Y, crs = crs), by = group]

test_that('arguments provided correctly else error', {
  expect_error(
    DT[, calc_centroid(x = X, geometry = geometry)],
    'arguments incorrectly provided'
  )

  expect_error(
    DT[, calc_centroid(y = Y, geometry = geometry)],
    'arguments incorrectly provided'
  )
})

test_that('sf is returned', {
  expect_s3_class(DT[, calc_centroid(geometry)], 'sf')
  expect_s3_class(DT[, .(centroid = calc_centroid(geometry))][[1]], 'sfc')
  expect_s3_class(
    DT[, calc_centroid(x = lonlat_X, y = lonlat_Y, crs = crs_lonlat)],
    'sf'
  )
  expect_s3_class(
    DT[, .(centroid = calc_centroid(x = X, y = Y, crs = crs))][[1]],
    'sfc'
  )

  expect_s3_class(
    DT[, calc_centroid(x = X, y = Y, crs = crs), by = ID],
    'data.table'
  )

  expect_s3_class(
    DT[, centroid := calc_centroid(x = X, y = Y, crs = crs), by = ID]$centroid,
    'sfc'
  )
})

test_that('expected dims returned', {
  expect_length(DT[, calc_centroid(geometry)], 1L)
  expect_length(DT[, calc_centroid(x = lonlat_X, y = lonlat_Y, crs = crs_lonlat)],
                1L)
})

test_that('calc_centroid equals st_centroid for mean and length 1 inputs', {
  new_nms <- c('X', 'Y')

  i <- DT[, sample(.I, 1)]
  i_seq <- DT[, sample(.I, 100)]

  expect_equal(
    setnames(DT[i_seq, calc_centroid(x = X, y = Y, crs = crs, use_mean = TRUE)], new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i_seq, .(X, Y)], coords = seq.int(2), crs = crs)))
    ))
  )

  expect_equal(
    setnames(DT[i, calc_centroid(x = X, y = Y, crs = crs, use_mean = TRUE)], new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i, .(X, Y)], coords = seq.int(2), crs = crs)))
    ))
  )

  expect_equal(
    setnames(DT[i_seq, calc_centroid(x = X, y = Y, crs = NA_crs_, use_mean = TRUE)], new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i_seq, .(X, Y)], coords = seq.int(2), crs = NA_crs_)))
    ))
  )

  expect_equal(
    setnames(DT[i, calc_centroid(x = X, y = Y, crs = crs, use_mean = TRUE)], , new = new_nms),
    data.frame(st_coordinates(
      st_centroid(st_combine(st_as_sf(DT[i, .(X, Y)], coords = seq.int(2), crs = NA_crs_)))
    ))
  )

})

# These specific test results require recent commits to sf. Save for later.
# test_that('NAs returned as expected', {
#   X_NA <- copy(DT)[seq.int(100)][sample(.N, 10), X := NA]
#   res <- X_NA[, calc_centroid(x = X, y = Y, crs = crs)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(sf::st_coordinates(res))))
#
#   Y_NA <- copy(DT)[seq.int(100)][sample(.N, 10), Y := NA]
#   res <- Y_NA[, calc_centroid(x = X, y = Y, crs = crs)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(sf::st_coordinates(res))))
#
#   XY_NA <- copy(DT)[seq.int(100)][sample(.N, 10), (lonlat_coords) := NA]
#   res <- XY_NA[, calc_centroid(x = lonlat_X, y = lonlat_Y, crs = 4326)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(sf::st_coordinates(res))))
#
#   get_geometry(XY_NA, lonlat_coords, crs_lonlat)
#   res <- XY_NA[, calc_centroid(geometry)]
#   expect_length(res, 1L)
#   expect_false(any(is.na(sf::st_coordinates(res))))
# })
