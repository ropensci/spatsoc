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

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
get_geometry(DT, coords = coords, crs = crs)

DT[, dest_geometry := sf::st_centroid(sf::st_union(geometry))]
DT[, calc_direction(geometry, dest_geometry)]

# Note: due to st_geod_azimuth not accepting null geometries
DT[!sf::st_is_empty(geometry) &
        !sf::st_is_empty(centroid),
      direction := calc_direction(geometry_a = geometry, geometry_b = centroid)]
DT[, direction_XY := calc_direction(x_a = X, y_a = Y, x_b = centroid_X, y_b = centroid_Y)]

DT[!sf::st_is_empty(geometry),
      direction_self := c(calc_direction(geometry_a = geometry),
                          units::as_units(NA, 'rad')),
      by = ID]
DT[, direction_XY_self := c(calc_direction(x_a = X, y_a = Y),
                               NA),
      by = ID]

DT[sample(.N, 10)]

# TODO: needs warning if combination are provided?. but cant/shouldnt if running by

# TODO: tests
# - is vect of units out
# - distances by st_distance and stats::dist are reasonably similar within tol
