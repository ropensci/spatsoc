# Test group_lines
context('test group_lines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <-
  '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

DT[, datetime := as.POSIXct(datetime)]
DT[, jul := data.table::yday(datetime)]
DT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]

test_that('one of DT or spLines is required, not both or neither', {
  expect_error(
    group_lines(
      DT = NULL,
      threshold = 10,
      spLines = NULL
    ),
    'must provide either DT or spLines'
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = 10,
      spLines = build_lines(
        DT,
        projection = utm,
        coordFields = c('X', 'Y'),
        idField = 'ID'
      )
    ),
    'cannot provide both DT and spLines'
  )
})


test_that('coordFields, idField, projection provided and proper format', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = NULL,
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'idField must be provided'
  )

  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = NULL
    ),
    'projection must be provided',
    fixed = FALSE
  )

  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = NULL,
      projection = utm
    ),
    'coordFields must be provided'
  )

})


test_that('column names must exist in DT', {
  expect_error(
    group_lines(
      DT = DT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'potatoID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('potatoX', 'potatoY'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )
})


test_that('threshold is correctly provided, or error', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]
  expect_warning(
    group_lines(
      DT = copyDT[N > 2],
      threshold = NULL,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'threshold missing, using 0 by default'
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = -10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'cannot provide a negative threshold'
  )
})


test_that('spLines provided must be an S4 + spatial lines', {
  expect_error(
    group_lines(spLines = DT, threshold = 10),
    'spLines provided must be a SpatialLines object'
  )
})

test_that('group lines returns a single warning for <2 locs', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '2 days')
  expect_warning(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'some rows were dropped, cannot build a line with',
    fixed = FALSE
  )

  # expect equal sum < 2
  # copyDT <- copy(DT)
  # group_times(copyDT, timeField = 'datetime', threshold = '2 days')
  # expect_equal(length(group_lines(DT = copyDT, threshold = 10,
  #                                timegroup = 'timegroup',
  #                                idField = 'ID', coordFields = c('X', 'Y'),
  #                                projection = utm))[!is.na(group)],
  #              sum(copyDT[, .N >= 2, by = timegroup]))
})

test_that('group column is added to result', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]

  expect_true('group' %in%
                colnames(
                  group_lines(
                    DT = copyDT[N > 2],
                    threshold = 10,
                    timegroup = 'timegroup',
                    idField = 'ID',
                    coordFields = c('X', 'Y'),
                    projection = utm
                  )
                ))
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]

  expect_equal(ncol(copyDT[N > 2]) + 1,
               ncol(
                 group_lines(
                   DT = copyDT[N > 2],
                   threshold = 10,
                   timegroup = 'timegroup',
                   idField = 'ID',
                   coordFields = c('X', 'Y'),
                   projection = utm
                 )
               ))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]
  copyDT <- copyDT[N > 2]
  expect_equal(nrow(copyDT),
               nrow(
                 group_lines(
                   DT = copyDT,
                   threshold = 10,
                   timegroup = 'timegroup',
                   idField = 'ID',
                   coordFields = c('X', 'Y'),
                   projection = utm
                 )
               ))
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)
  group_times(copyDT, timeField = 'datetime', threshold = '14 days')
  copyDT[, N := .N, by = .(ID, block)]
  expect_false('withinGroup' %in% colnames(
    group_lines(
      DT = copyDT[N > 2],
      threshold = 10,
      timegroup = 'timegroup',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    )
  ))
})

test_that('only 1 unique timegroup * groupFields', {
  copyDT <- DT[, mnth := month(datetime)][, yr := year(datetime)]

  group_lines(
    DT = copyDT,
    threshold = 100,
    timegroup = 'mnth',
    idField = 'ID',
    coordFields = c('X', 'Y'),
    projection = utm,
    groupFields = 'yr'
  )
  expect_equal(copyDT[, .(uniqueMonths = uniqueN(mnth)),
                      by = .(group)][, max(uniqueMonths)],
               1)

  expect_equal(copyDT[, .(uniqueYears = uniqueN(yr)),
                      by = .(group)][, max(uniqueYears)],
               1)

})
# or uniquen(.SD, by = c(idField, groupFields))


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1]
  copyDT[, mnth := month(datetime)][, yr := year(datetime)]

  expect_warning(
    group_lines(
      DT = copyDT,
      threshold = 100,
      timegroup = 'mnth',
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm,
      groupFields = 'yr'
    ),
    'group column will be overwritten'
  )
})

# group_lines(
#   DT = DT,
#   threshold = 10,
#   timeField = 'timegroup',
#   groupFields = 'family',
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
