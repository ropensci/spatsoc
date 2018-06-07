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
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_warning(GroupTimes(copyDT, timeField = 'datetime', threshold = NULL),
                 'no threshold provided', fixed = FALSE)
})


test_that('time fields are already present', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  GroupTimes(copyDT, timeField = 'datetime', threshold = '10 minutes')
  expect_warning(GroupTimes(copyDT, timeField = 'datetime', threshold = '10 minutes'),
                 'columns found in input DT', fixed = FALSE)
})

test_that('time field is appropriate format', {
  # where character is provided
  copyDT <- copy(DT)
  expect_error(GroupTimes(copyDT, timeField = 'datetime', threshold = '60 minutes'),
               'time field provided must be either', fixed = FALSE)

  # where numeric is provided
  copyDT <- copy(DT)
  copyDT[, datetimenumeric := 1]
  expect_error(GroupTimes(copyDT, timeField = 'datetimenumeric',
                          threshold = '60 minutes'),
               'time field provided must be either', fixed = FALSE)
})

test_that('threshold with minutes fails with > 60', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(GroupTimes(copyDT, timeField = 'datetime', threshold = '70 minutes'),
               '> 60 minutes', fixed = FALSE)
})

test_that('threshold with minutes fails if not divisible by 60', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(GroupTimes(copyDT, timeField = 'datetime', threshold = '13 minutes'),
               'threshold not evenly', fixed = FALSE)
})

test_that('threshold provided must be in units of hours, minutes, days', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(GroupTimes(copyDT, timeField = 'datetime', threshold = '13 potatoes'),
               'must provide threshold in units', fixed = FALSE)
})

test_that('check that 60 minutes and 1 hour are the same result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  expect_equal({
    copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
    GroupTimes(copyDT, timeField = 'datetime',
               threshold = '1 hour')
  },
  {
    copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
    GroupTimes(copyDT, timeField = 'datetime',
               threshold = '60 minutes')
  })
})

test_that('warns if block is not even', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  if (copyDT[, max(data.table::yday(datetime)) -
             min(data.table::yday(datetime))] %% 13 == 0) {
    blockLength <- '17 days'
  } else {
    blockLength <- '13 days'
  }

  expect_warning(GroupTimes(copyDT, timeField = 'datetime',
                            threshold = blockLength),
                 'the minimum and maximum days in DT', fixed = FALSE)

})


test_that('timegroup column + time fields are added to result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true('timegroup' %in%
                colnames(
                  GroupTimes(copyDT, timeField = 'datetime', threshold = '1 day')
                ))

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true(all(c('timegroup', 'block') %in%
                colnames(
                  GroupTimes(copyDT, timeField = 'datetime', threshold = '2 days')
                )))

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true(all(c('timegroup', 'hours') %in%
                    colnames(
                      GroupTimes(copyDT, timeField = 'datetime',
                                 threshold = '2 hours')
                    )))

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true(all(c('timegroup', 'minutes') %in%
                    colnames(
                      GroupTimes(copyDT, timeField = 'datetime',
                                 threshold = '10 minutes')
                    )))
})
