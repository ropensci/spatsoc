# Test GroupTimes
context('test GroupTimes')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')


test_that('DT is required', {
  expect_error(GroupTimes(DT = NULL, timeField = NULL, threshold = '10 minutes'),
               'input DT required')
})


test_that('time field correctly provided or error detected', {
  expect_error(GroupTimes(DT, timeField = NULL, threshold = '10 minutes'),
               'time field required')

  expect_error(GroupTimes(DT, timeField = 'potato', threshold = '10 minutes'),
               'time field provided is not found in DT')
})

test_that('if threshold is null, warning returned', {
  copyDT <- copy(DT)[, posix := as.POSIXct(posix)]
  expect_warning(GroupTimes(copyDT, timeField = 'posix', threshold = NULL),
                 'no threshold provided', fixed = FALSE)
})
