# Test calc_distance
context('test calc_distance')

DT <- fread('../testdata/DT.csv')
coords <- c('X', 'Y')
crs <- 32736
crs_lonlat <- 4326

# Reduce test data size
# DT <- DT[ID %in% c('A', 'B', 'C')]

get_geometry(DT, coords = coords, crs = crs)

DT[, dest_geometry := sf::st_centroid(sf::st_union(geometry))]

lonlat_coords <- paste0('lonlat_', coords)
DT[, (lonlat_coords) := as.data.table(sf::st_coordinates(geometry))]

dest_coords <- paste0('dest_', coords)
DT[, (dest_coords) := as.data.table(sf::st_coordinates(dest_geometry))]

# DT[, calc_distance(geometry_a = geometry, geometry_b = dest_geometry)]
# DT[, calc_distance(x_a = lonlat_X, y_a = lonlat_Y,
#                     x_b = dest_X, y_b = dest_Y,
#                     crs = crs_lonlat)]

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

test_that('units are returned', {
  expect_s3_class(DT[, calc_distance(geometry)], 'units')
  expect_s3_class(
    DT[, calc_distance(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
    'units'
  )
  expect_s3_class(
    DT[, calc_distance(x_a = X, y_a = Y, crs = crs)],
    'units'
  )
})

test_that('expected dims returned', {
  N <- 10
  expect_length(DT[seq.int(N), calc_distance(geometry)], N * N)
  expect_length(DT[seq.int(N), calc_distance(geometry, dest_geometry)], N)

  expect_length(
    DT[seq.int(N), calc_distance(x_a = lonlat_X, y_a = lonlat_Y, crs = crs_lonlat)],
    N * N
  )
  expect_length(
    DT[seq.int(N), calc_distance(x_a = lonlat_X, y_a = lonlat_Y,
                                 x_b = dest_X, y_b = dest_Y,
                                 crs = crs_lonlat)],
    N
  )
})
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

