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
group_times(DT, datetime = 'datetime', threshold = '20 minutes')
group_pts(DT, threshold = 50, id = 'ID', coords = c('X', 'Y'), timegroup = 'timegroup')

test_that('DT is required', {
  expect_error(centroid_group(DT = NULL))
})
