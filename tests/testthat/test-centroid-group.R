# Test centroid_group
context('test centroid_group')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id, coords = coords, timegroup = timegroup)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(centroid_group(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(centroid_group(DT, coords = 'X'),
               'coords requires a vector')
  expect_error(centroid_group(DT, coords = coords, group = NULL),
               'group column name required')
  expect_error(centroid_group(DT, coords = coords, na.rm = NULL),
               'na.rm is required')
})

test_that('column names must exist in DT', {
  expect_error(centroid_group(DT, coords = rep('potato', 2)),
               'potato field')
  expect_error(centroid_group(DT, coords = coords, group = 'potato'),
               'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(centroid_group(DT, coords = c('X', NULL)),
               'coords requires a vector')
  expect_error(centroid_group(DT, coords = c('X', 'ID')),
               'coords must be numeric')
})
