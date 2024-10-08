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
projection <- 32736

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
group_pts(DT, threshold = threshold, id = id, coords = coords, timegroup = timegroup)
direction_step(DT, id = id, coords = coords, projection = projection)

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
