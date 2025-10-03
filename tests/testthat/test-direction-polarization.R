# Test direction_polarization
context('test direction_polarization')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
utm <- 32736

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
direction_step(DT, id = id, coords = coords, crs = utm)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_polarization(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(direction_polarization(DT, group = NULL),
               'group column name required')
  expect_error(direction_polarization(DT, direction = NULL),
               'direction column name required')
})

test_that('column names must exist in DT', {
  expect_error(direction_polarization(DT, direction = 'potato'),
               'potato field')
  expect_error(direction_polarization(DT, group = 'potato'),
               'potato field')
})

test_that('radians expected else error', {
  expect_error(direction_polarization(DT, direction = 'X'),
               'direction_step')
})

test_that('direction expected numeric', {
  expect_error(direction_polarization(DT, direction = 'ID'),
               'direction must be numeric')
})


test_that('polarization column succesfully detected', {
  copyDT <- copy(clean_DT)[, polarization := 1]
  expect_message(
    direction_polarization(copyDT),
    'polarization column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(direction_polarization(copyDT)))
})

test_that('one column added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 1,
               ncol(direction_polarization(DT)))
})

test_that('column added to the result DT is numeric, between 0-1', {
  expect_type(direction_polarization(DT)$polarization, 'double')
  expect_gte(min(direction_polarization(DT)$polarization, na.rm = TRUE), 0)
  expect_lte(max(direction_polarization(DT)$polarization, na.rm = TRUE), 1)
})

test_that('returns a data.table', {
  expect_s3_class(direction_polarization(DT), 'data.table')
})
