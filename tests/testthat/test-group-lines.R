# Test group_lines
context('test group_lines')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

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
        coords = c('X', 'Y'),
        id = 'ID',
        sortBy = 'datetime'
      )
    ),
    'cannot provide both DT and spLines'
  )
})


test_that('coords, id, projection provided and proper format', {
  copyDT <- copy(DT)
  group_times(copyDT, datetime = 'datetime', threshold = '1 day')
  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      id = NULL,
      coords = c('X', 'Y'),
      projection = utm
    ),
    'id must be provided'
  )

  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
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
      id = 'ID',
      coords = NULL,
      projection = utm
    ),
    'coords must be provided'
  )

  expect_error(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = NULL
    ),
    'sortBy must be provided'
  )

})


test_that('column names must exist in DT', {
  expect_error(
    group_lines(
      DT = DT,
      threshold = 10,
      timegroup = 'timegroup',
      id = 'potatoID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = 10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})



test_that('timegroup is correctly provided but is not required', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]
  expect_error(
    group_lines(
      DT = copyDT[N > 2],
      threshold = 10,
      timegroup = 'potato',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'provided are not present', fixed = FALSE
  )

  expect_true('data.table' %in% class(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = NULL,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ))
  )
})



test_that('threshold is correctly provided, or error', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]
  expect_message(
    group_lines(
      DT = copyDT[N > 2],
      threshold = NULL,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'threshold missing, using 0 by default'
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = -10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'cannot provide a negative threshold'
  )

  expect_silent(
    group_lines(
      DT = copyDT[N > 2],
      threshold = 0,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    )
  )

  expect_error(
    group_lines(
      DT = DT,
      threshold = '0',
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'threshold must be numeric'
  )
})


test_that('group lines returns a single warning for <2 locs', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )

  copyDT[1, ID := 'Z']
  expect_warning(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'some rows were dropped, cannot build a line with',
    fixed = FALSE
  )

  copyDT <- copy(DT)
  copyDT[1, ID := 'Z']
  expect_warning(
    group_lines(
      DT = copyDT,
      threshold = 10,
      timegroup = NULL,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'some rows were dropped, cannot build a line with', fixed = FALSE
  )

})

test_that('group column is added to result or NA if < 2 locs', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]

  expect_true('group' %in%
                colnames(
                  group_lines(
                    DT = copyDT[N > 2],
                    threshold = 10,
                    timegroup = 'timegroup',
                    id = 'ID',
                    coords = c('X', 'Y'),
                    projection = utm,
                    sortBy = 'datetime'
                  )
                ))

  copyDT <- copy(DT)
  copyDT[1, ID := 'Z']
  expect_true(suppressWarnings(
    group_lines(
      DT = copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    )[is.na(group), .N] != 0)
  )

  copyDT <- DT[, .SD[1], by = ID]
  expect_true(suppressWarnings(
    group_lines(
      DT = copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    )[is.na(group), .N] != 0)
  )
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]

  expect_equal(ncol(copyDT[N > 2]) + 1,
               ncol(
                 group_lines(
                   DT = copyDT[N > 2],
                   threshold = 10,
                   timegroup = 'timegroup',
                   id = 'ID',
                   coords = c('X', 'Y'),
                   projection = utm,
                   sortBy = 'datetime'
                 )
               ))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]
  copyDT <- copyDT[N > 2]
  expect_equal(nrow(copyDT),
               nrow(
                 group_lines(
                   DT = copyDT,
                   threshold = 10,
                   timegroup = 'timegroup',
                   id = 'ID',
                   coords = c('X', 'Y'),
                   projection = utm,
                   sortBy = 'datetime'
                 )
               ))
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, timegroup)]
  expect_false('withinGroup' %in% colnames(
    group_lines(
      DT = copyDT[N > 2],
      threshold = 10,
      timegroup = 'timegroup',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    )
  ))
})

test_that('only 1 unique timegroup * splitBy', {
  copyDT <- DT[, mnth := month(datetime)][, yr := year(datetime)]
  copyDT[, nBy := .N, by = .(yr, mnth, ID)]
  copyDT <- copyDT[nBy > 2]
  group_lines(
    DT = copyDT,
    threshold = 100,
    timegroup = 'mnth',
    id = 'ID',
    coords = c('X', 'Y'),
    projection = utm,
    splitBy = 'yr',
    sortBy = 'datetime'
  )
  expect_equal(copyDT[, .(uniqueMonths = uniqueN(mnth)),
                      by = .(group)][, max(uniqueMonths)],
               1)

  expect_equal(copyDT[, .(uniqueYears = uniqueN(yr)),
                      by = .(group)][, max(uniqueYears)],
               1)

})
# or uniquen(.SD, by = c(id, splitBy))


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1]
  copyDT[, mnth := month(datetime)][, yr := year(datetime)]
  copyDT[, nBy := .N, by = .(yr, mnth, ID)]
  copyDT <- copyDT[nBy > 2]

  expect_message(
    group_lines(
      DT = copyDT,
      threshold = 0,
      timegroup = 'mnth',
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'yr',
      sortBy = 'datetime'
    ),
    'group column will be overwritten'
  )

  expect_message(
    group_lines(
      DT = copyDT,
      threshold = 0,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'yr',
      sortBy = 'datetime'
    ),
    'group column will be overwritten'
  )
})


# group_lines(
#   DT = DT,
#   threshold = 10,
#   datetime = 'timegroup',
#   splitBy = 'family',
#   id = 'ID',
#   coords = c('X', 'Y'),
#   projection = utm
# )


test_that('spLines provided must be an S4 + spatial lines', {
  expect_error(
    group_lines(spLines = DT, threshold = 10),
    'spLines provided must be a SpatialLines object'
  )
})

test_that('spLines provided returns data.table', {
  spLines <- build_lines(
    DT = DT,
    id = 'ID',
    coords = c('X', 'Y'),
    projection = utm,
    sortBy = 'datetime'
  )

  expect_true(
    'data.table' %in%
      class(group_lines(spLines = spLines, threshold = 10))
  )

  expect_true(
    'data.table' %in%
      class(group_lines(spLines = spLines, threshold = 0))
  )

  expect_equal(
    nrow((group_lines(spLines = spLines, threshold = 10))),
    length(spLines)
  )

})


