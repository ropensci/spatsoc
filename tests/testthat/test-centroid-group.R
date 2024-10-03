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

test_that('na.rm is boolean', {
  expect_error(centroid_group(DT, coords = coords, na.rm = 'potato'),
               'boolean')
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

test_that('centroid column succesfully detected', {
  copyDT <- copy(clean_DT)[, centroid_X := 1]
  expect_message(
    centroid_group(copyDT, coords = coords),
    'centroid_X column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(centroid_group(copyDT, coords = coords)))
})

test_that('two columns added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 2,
               ncol(centroid_group(DT, coords = coords)))
})

test_that('two columns added to the result DT', {
  expect_type(centroid_group(DT, coords = coords)$centroid_X, 'double')
  expect_type(centroid_group(DT, coords = coords)$centroid_Y, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(centroid_group(DT, coords = coords), 'data.table')
})


DT <- data.table(
  group = c(1, 1, 2, 2),
  X = c(10, 20, 10, NA),
  Y = c(10, 20, 10, 20)
)

test_that('results are expected', {
  expect_equal(
    centroid_group(copy(DT), coords, na.rm = FALSE)[group == 1, unique(centroid_X)],
    15
  )

  expect_equal(
    centroid_group(copy(DT), coords, na.rm = FALSE)[group == 2, unique(centroid_X)],
    NA_real_
  )

  expect_equal(
    centroid_group(copy(DT), coords, na.rm = TRUE)[group == 1, unique(centroid_X)],
    15
  )

  expect_equal(
    centroid_group(copy(DT), coords, na.rm = TRUE)[group == 2, unique(centroid_X)],
    10
  )
})

