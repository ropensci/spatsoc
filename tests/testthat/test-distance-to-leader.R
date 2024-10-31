# Test distance_to_leader
context('test distance_to_leader')

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

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
centroid_group(DT, coords = coords, group = group, na.rm = TRUE)
direction_step(DT = DT, id = id, coords = coords, projection = projection)
direction_group(DT)
leader_direction_group(DT, coords = coords, group = group, return_rank = TRUE)

# Removing group with missing leader
DT <- copy(DT)[group != 868]

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(distance_to_leader(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(distance_to_leader(DT, coords = NULL),
               'coords req')
  expect_error(distance_to_leader(DT, coords = coords, group = NULL),
               'group column name required')
})

test_that('column names must exist in DT', {
  expect_error(distance_to_leader(DT, coords = rep('potato', 2), group = group),
               'potato field')
  expect_error(distance_to_leader(DT, coords = coords, group = 'potato'),
               'group column')
  copy_DT <- copy(DT)
  setnames(copy_DT, 'rank_position_group_direction', 'potato')
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'did you run leader?')
})

test_that('coords are correctly provided or error detected', {
  expect_error(distance_to_leader(DT, coords = c('X', NULL), group = group),
               'coords requires a vector')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'coords must be numeric')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords,
                                  group = group),
               'coords must be numeric')
})

test_that('message when distance_leader column overwritten', {
  copyDT <- copy(clean_DT)[, distance_leader := 1]
  expect_message(
    distance_to_leader(copyDT, coords = coords, group = group),
    'distance_leader column will be overwritten'
  )
})
