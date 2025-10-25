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

