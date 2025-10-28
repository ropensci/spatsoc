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
# Note: due to https://github.com/Rdatatable/data.table/issues/4415
#  need to recompute the bbox
# DT_sf[, centroid := st_sfc(centroid, recompute_bbox = TRUE)]

test_that('arguments provided correctly else error', {
  expect_error(
    DT[, calc_centroid(x = X, geometry = geometry)]
  )

  expect_error(
    DT[, calc_centroid(y = Y, geometry = geometry)]
  )
})
