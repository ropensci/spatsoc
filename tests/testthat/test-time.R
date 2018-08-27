# Test group_times
context('test group_times')
library(spatsoc)

DT <- fread('../testdata/DT.csv')
# Sys.setenv(TZ='GMT')

test_that('DT is required', {
  expect_error(group_times(DT = NULL,
                           datetime = NULL, threshold = '10 minutes'),
               'input DT required')
})


test_that('time field correctly provided or error detected', {
  expect_error(group_times(DT, datetime = NULL, threshold = '10 minutes'),
               'datetime field required')

  expect_error(group_times(DT,
                           datetime = 'potato',
                           threshold = '10 minutes'),
               'time field provided is not found in DT')
})

test_that('threshold properly provided', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_message(
    group_times(copyDT, datetime = 'datetime',
                threshold = NULL),
    'no threshold provided',
    fixed = FALSE
  )

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(
    group_times(copyDT, datetime = 'datetime',
                threshold = '50 potato'),
    'must provide threshold in units of hour, day, or minute'
  )
})


test_that('time fields are already present', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  expect_message(group_times(copyDT, datetime = 'datetime',
                             threshold = '10 minutes'),
                 'columns found in input DT', fixed = FALSE)
})

test_that('time field is appropriate format', {
  # where character is provided
  copyDT <- copy(DT)
  expect_error(
    group_times(copyDT, datetime = 'datetime', threshold = '60 minutes'),
    'time field provided must be either',
    fixed = FALSE
  )

  # where numeric is provided
  copyDT <- copy(DT)
  copyDT[, datetimenumeric := 1]
  expect_error(group_times(copyDT, datetime = 'datetimenumeric',
                          threshold = '60 minutes'),
               'time field provided must be either', fixed = FALSE)


})

test_that('threshold with minutes fails with > 60', {
  copyDT <- copy(DT)[, c('idate', 'itime') := IDateTime(datetime)]
  expect_error(group_times(copyDT, datetime = c('idate', 'itime'),
                           threshold = '70 minutes'),
               '> 60 minutes', fixed = FALSE)
})

test_that('threshold with minutes fails if not divisible by 60', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(
    group_times(copyDT, datetime = 'datetime', threshold = '13 minutes'),
    'threshold not evenly',
    fixed = FALSE
  )
})

test_that('threshold provided must be in units of hours, minutes, days', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(group_times(copyDT,
                           datetime = 'datetime',
                           threshold = '13 potatoes'),
               'must provide threshold in units', fixed = FALSE)
})

test_that('check that 60 minutes and 1 hour are the same result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  expect_equal({
    copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
    group_times(copyDT, datetime = 'datetime',
               threshold = '1 hour')
  },
  {
    copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
    group_times(copyDT, datetime = 'datetime',
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

  expect_warning(group_times(copyDT, datetime = 'datetime',
                            threshold = blockLength),
                 'the minimum and maximum days', fixed = FALSE)

})


test_that('timegroup column + time fields are added to result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true('timegroup' %in%
                colnames(group_times(
                  copyDT, datetime = 'datetime',
                  threshold = '1 day'
                )))

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  # to avoid block length warning
  expect_true(all(c('timegroup', 'block') %in%
                    colnames(suppressWarnings(
                      group_times(copyDT,
                                  datetime = 'datetime',
                                  threshold = '2 days')
                    ))))
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  copyDT <- copyDT[year(datetime) == unique(year(datetime))[1]]
  expect_true(all(c('timegroup', 'block') %in%
                    colnames(
                      group_times(copyDT,
                                  datetime = 'datetime',
                                  threshold = '2 days')
                    )))


  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true(all(c('timegroup', 'hours') %in%
                    colnames(
                      group_times(copyDT, datetime = 'datetime',
                                 threshold = '2 hours')
                    )))

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_true(all(c('timegroup', 'minutes') %in%
                    colnames(
                      group_times(copyDT, datetime = 'datetime',
                                 threshold = '10 minutes')
                    )))
})

test_that('timegroup column and fields are detected if already present', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '2 days'))

  expect_message(
    group_times(copyDT, datetime = 'datetime', threshold = '1 day'),
    'block, timegroup ',
    fixed = FALSE
  )

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')

  expect_message(
    group_times(copyDT, datetime = 'datetime', threshold = '10 minutes'),
    'minutes, timegroup ',
    fixed = FALSE
  )

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '2 hours')

  expect_message(
    group_times(copyDT, datetime = 'datetime', threshold = '2 hours'),
    'hours, timegroup ',
    fixed = FALSE
  )
})

test_that('warns if no threshold provided', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  expect_message(
    group_times(copyDT, datetime = 'datetime'),
    'no threshold provided',
    fixed = FALSE
  )
})

test_that('warns if threshold is fractional', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  expect_warning(
    group_times(copyDT, datetime = 'datetime', threshold = '2.5 hours'),
    'number of hours provided cannot be a fractional',
    fixed = FALSE
  )

  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_warning(
    group_times(copyDT, datetime = 'datetime', threshold = '2.5 minutes'),
    'number of minutes provided cannot be a fractional',
    fixed = FALSE
  )

})

test_that('error if threshold not divisible by 24 hours', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]

  expect_error(
    group_times(copyDT, datetime = 'datetime', threshold = '5 hours'),
    'does not evenly divide into 24',
    fixed = FALSE
  )

})

test_that('multiyear blocks are well handled', {
  copyDT <- copy(DT)
  copyDT[, isoDate := as.POSIXct(
    ISOdatetime(2006, 10, 10, 10, 10, 10)
  )]
  expect_equal(group_times(copyDT, datetime = 'isoDate',
                           threshold = '1 day')[, .N,
                                                by = timegroup]$N,
               copyDT[, .N,
                by = .(data.table::yday(isoDate))]$N)

  copyDT[1, isoDate := as.POSIXct(
    ISOdatetime(2010, 10, 10, 10, 10, 10)
  )]
  copyDT[, timegroup := NULL]
  expect_equal(group_times(copyDT, datetime = 'isoDate',
                           threshold = '1 day')[, .N,
                                                by = timegroup]$N,
               copyDT[, .N, by = .(data.table::yday(isoDate),
                                   data.table::year(isoDate))]$N)

})
