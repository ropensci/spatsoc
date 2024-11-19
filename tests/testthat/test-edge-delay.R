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
                   timegroup = timegroup, returnDist = TRUE, fillNA = FALSE)
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

