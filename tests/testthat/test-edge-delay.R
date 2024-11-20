# Test edge_delay
context('test edge_delay')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736
window <- 3


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
direction_step(DT, id, coords, projection)
edges <- edge_dist(DT, threshold = threshold, id = id,
                   coords = coords, timegroup = timegroup,
                   returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
fusion_id(edges, threshold = threshold)

clean_DT <- copy(DT)
clean_edges <- copy(edges)

# edge_delay(DT = DT, edges = edges, id = id, window = window)

test_that('edges, DT are required', {
  expect_error(edge_delay(edges, DT = NULL))
  expect_error(edge_delay(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(edge_delay(edges, DT, id = NULL),
               'id column name required')
  expect_error(edge_delay(edges, DT, id = id, window = NULL),
               'window is required')
})

test_that('window is numeric', {
  expect_error(edge_delay(edges, DT, id = id, window = 'potato'),
               'numeric')
})

test_that('column names must exist in DT', {
  expect_error(edge_delay(edges, DT, id = 'potato'),
               'potato field')

  copy_edges <- copy(clean_edges)
  copy_edges[, timegroup := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'timegroup field')

  copy_edges <- copy(clean_edges)
  copy_edges[, fusionID := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'fusionID field')

  copy_edges <- copy(clean_edges)
  copy_edges[, dyadID := NULL]
  expect_error(edge_delay(copy_edges, DT, id = id, window = window),
               'dyadID field')

  expect_error(edge_delay(edges, DT, id = id, direction = 'potato'),
               'direction field')

  copy_DT <- copy(clean_DT)
  copy_DT[, timegroup := NULL]
  expect_error(edge_delay(edges, copy_DT, id = id, window = window),
               'timegroup field')
})

test_that('no rows are added to the result edges', {
  expect_equal(nrow(edges),
               nrow(edge_delay(edges, DT, id = id, window = window)))
})

test_that('two columns added to the result DT', {
  copyEdges <- copy(edges)

  expect_equal(length(c('ID1', 'ID2', 'timegroup',
                        'dyadID', 'fusionID', 'dir_corr_delay')),
               ncol(edge_delay(edges, DT, id = id, window = window)))
})

test_that('column added to the result DT is double', {
  expect_type(edge_delay(edges, DT, id = id, window = window)$dir_corr_delay, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(edge_delay(edges, DT, id = id, window = window), 'data.table')
})

