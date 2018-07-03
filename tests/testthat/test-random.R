# Test Random
context('test randomizations')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

Dt[, datetime := as.POSIXct(datetime)]
Dt[, yr := year(datetime)]

group_times(Dt, datetime = 'datetime', threshold = '2 hours')
group_pts(Dt, threshold = 100, id = 'id', timegroup = 'timegroup',
          coords = c('X', 'Y'))


test_that('DT is required', {
  expect_error(randomizations(DT = NULL),
  'input DT required')
})
