# Test Random
context('test randomizations')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

group_times(DT, datetime = 'datetime', threshold = '2 hours')
group_pts(DT, threshold = 100, id = 'ID', timegroup = 'timegroup',
          coords = c('X', 'Y'))


test_that('DT, type, id, datetime are required', {
  expect_error(randomizations(DT = NULL),
  'input DT required')

  expect_error(randomizations(DT = DT,
                              type = NULL),
               'type of randomization', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = NULL),
               'id field required')

  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'ID',
                              datetime = NULL),
               'datetime field required')

})

test_that('type must be one of options', {
  expect_error(randomizations(DT = DT,
                              type = 'potato'),
               'type of randomization must be one of: hourly, daily or trajectory')
})

test_that('fields provided must be in DT', {
  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'potato',
                              datetime = 'datetime'),
               'field(s) provided are not present', fixed = TRUE)

  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'ID',
                              datetime = 'potato'),
               'field(s) provided are not present', fixed = TRUE)
})

test_that('iterations is NULL or correctly provided', {
  expect_warning(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'ID',
                              datetime = 'datetime',
                              iterations = NULL),
               'iterations is not', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'hourly',
                              id = 'ID',
                              datetime = 'datetime',
                              iterations = 'potato'),
               'either provide a numeric for iterations or NULL', fixed = FALSE)
})

