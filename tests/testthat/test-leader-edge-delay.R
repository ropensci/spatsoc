# Test leader_edge_delay
context('test leader_edge_delay')

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
delay <- edge_delay(edges, DT, id = id, window = window)
leader_delay <- leader_edge_delay(delay, threshold = 0.5)

# leader_edge_delay(edges = edges, threshold = 0.5, splitBy = 'population')

clean_delay <- copy(delay)

test_that('edges required', {
  expect_error(leader_edge_delay(edges = NULL))
})

test_that('threshold is numeric', {
  expect_error(leader_edge_delay(delay, threshold = 'potato'),
               'numeric')
})

test_that('column names must exist in DT', {
  missing_delay <- copy(clean_delay)[, direction_diff := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'direction_diff')

  missing_delay <- copy(clean_delay)[, direction_delay := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'direction_delay')

  missing_delay <- copy(clean_delay)[, ID1 := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'ID1')

  missing_delay <- copy(clean_delay)[, ID2 := NULL]
  expect_error(leader_edge_delay(missing_delay),
               'ID2')

})

test_that('output length as expected', {
  # nrow(delay) is less than nrow(leader_delay) since leader_delay is aggregate
  expect_gt(nrow(delay), nrow(leader_delay))

  # nrow(delay) is less than when threshold is 1e-2 since some dyads will drop
  expect_gt(
    nrow(leader_edge_delay(delay)),
    nrow(leader_edge_delay(delay, threshold = 1e-2))
  )
  # nrow(delay) is the same when threshold is 1e3 since no rows will drop
  expect_equal(
    nrow(leader_edge_delay(delay)),
    nrow(leader_edge_delay(delay, threshold = 1e3))
  )

  # nrow(delay) with tiny threshold is 0 since all rows will drop
  expect_equal(nrow(leader_edge_delay(delay, threshold = 1e-3)), 0)
})
