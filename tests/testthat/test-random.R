# Test Random
context('test randomizations')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

group_times(DT, datetime = 'datetime', threshold = '2 hours')
group_pts(DT, threshold = 100, id = 'ID', timegroup = 'timegroup',
          coords = c('X', 'Y'))


test_that('DT, type, id are required', {
  expect_error(randomizations(DT = NULL),
  'input DT required')

  expect_error(randomizations(DT = DT,
                              type = NULL),
               'type of randomization', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = NULL),
               'ID field', fixed = FALSE)
})

test_that('fields provided must be in DT', {
  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'potato'),
               'id field provided are not present in input DT')
})
