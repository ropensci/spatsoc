# Test edge_zones
context('test edge_zones')

library(spatsoc)
library(units)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
projection <- 32736
timegroup <- 'timegroup'
group <- 'group'

zone_thresholds <- c(5, 20, 50)
zone_labels <- c('repulsion', 'orientation', 'attraction')
blind_volume <- 2

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                   timegroup = timegroup, returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

DT_blind <- copy(DT)
direction_step(DT_blind, id, coords, projection)
group_times(DT_blind, datetime = datetime, threshold = timethreshold)
edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                   timegroup = timegroup, returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
dyad_directions <- edge_direction(edges, DT_blind, id, coords,
                                  projection, timegroup)

# edge_zones(
#   edges = edges,
#   zone_thresholds = zone_thresholds,
#   zone_labels = zone_labels
# )

# edge_zones(
#   edges = dyad_directions,
#   zone_thresholds = zone_thresholds,
#   zone_labels = zone_labels,
#   blind_volume = blind_volume
# )

test_that('arguments required, otherwise error detected', {
  expect_error(
    edge_zones(
      edges = NULL,
      zone_thresholds = zone_thresholds,
      zone_labels = zone_labels
    ),
    'edges'
  )

  expect_error(
    edge_zones(
      edges = edges,
      zone_thresholds = NULL,
      zone_labels = zone_labels
    ),
    'zone_thresholds'
  )

  expect_error(
    edge_zones(
      edges = edges,
      zone_thresholds = zone_thresholds,
      zone_labels = NULL
    ),
    'zone_labels'
  )
})

test_that('columns must exist in edges', {
  rm_col <- copy(edges)[, .SD, .SDcols = -'distance']
  expect_error(edge_zones(rm_col, zone_thresholds, zone_labels),
               'distance')

  rm_col <- copy(dyad_directions)[, .SD, .SDcols = -'direction']
  expect_error(edge_zones(rm_col, zone_thresholds, zone_labels, blind_volume),
               'direction')

  rm_col <- copy(dyad_directions)[, .SD, .SDcols = -'direction_dyad']
  expect_error(edge_zones(rm_col, zone_thresholds, zone_labels, blind_volume),
               'direction_dyad')

})

test_that('columns are correctly provided or error detected', {
  char_col <- copy(edges)[, distance := as.character(distance)]
  expect_error(edge_zones(char_col, zone_thresholds, zone_labels),
               'distance must be numeric')

  char_col <- copy(dyad_directions)[, direction := as.character(direction)]
  expect_error(edge_zones(char_col, zone_thresholds, zone_labels, blind_volume),
               'radians',
              )

  char_col <- copy(dyad_directions)[, direction_dyad := as.character(direction_dyad)]
  expect_error(edge_zones(char_col, zone_thresholds, zone_labels, blind_volume),
               'radians')
})
