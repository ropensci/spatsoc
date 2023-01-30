context("test-edge-dist")

library(spatsoc)

DT <- fread('../testdata/DT.csv')

test_that('DT is required', {
  expect_error(edge_dist(
    DT = NULL,
    threshold = 10,
    id = 'ID'
  ),
  'input DT required')
})

test_that('ID and coords column names, threshold correctly provided',
          {
            expect_error(edge_dist(DT, threshold = 10, id = NULL),
                         'ID field required')

            expect_error(edge_dist(DT, id = 'ID'),
                         'threshold required')

            expect_error(
              edge_dist(
                DT,
                threshold = 10,
                id = 'ID',
                coords = 'X'
              ),
              'coords requires a vector',
              fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  # where ID field doesn't exist in DT
  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'potato',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where coords don't exist
  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      timegroup = 'timegroup'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where group fields doesn't exist
  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'timegroup',
      splitBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where timegroup field doesn't exist
  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})


test_that('threshold correctly provided or error detected', {
  copyDT <- copy(DT)

  expect_error(edge_dist(DT, threshold = -10, id = 'ID'),
               'threshold must be greater than 0')

  expect_error(edge_dist(DT, threshold = 0, id = 'ID'),
               'threshold must be greater than 0')

  expect_error(edge_dist(DT, threshold = '0', id = 'ID'),
               'threshold must be numeric')
})


test_that('coords are correctly provided or error detected', {
  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', NULL),
      timegroup = 'timegroup'
    ),
    'coords requires a vector'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'ID'),
      timegroup = NULL
    ),
    'coords must be numeric'
  )
})

test_that('warns if timegroup is a datetime or character',
          {
            # if datetime is a character
            copyDT <- copy(DT)
            expect_warning(
              edge_dist(
                copyDT,
                threshold = 10,
                id = 'ID',
                coords = c('X', 'Y'),
                timegroup = 'datetime'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )

            # if datetime is a POSIXct
            copyDT <- copy(DT)
            copyDT[, posix := as.POSIXct(datetime)]
            expect_warning(
              edge_dist(
                copyDT,
                threshold = 10,
                id = 'ID',
                coords = c('X', 'Y'),
                timegroup = 'posix'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )

            # if datetime is an IDate
            copyDT <- copy(DT)
            copyDT[, idate := as.IDate(datetime)]
            expect_warning(
              edge_dist(
                copyDT,
                threshold = 10,
                id = 'ID',
                coords = c('X', 'Y'),
                timegroup = 'idate'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )
          })

test_that('duplicate IDs in a timegroup detected', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '8 hours')
  expect_warning(edge_dist(
    copyDT,
    threshold = 10,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  ),
  'found duplicate id in a timegroup', fixed = FALSE)
})


test_that('returned IDs make sense', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  eDT <- edge_dist(
    copyDT,
    threshold = 50,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    fillNA = TRUE
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID1 %in% IDs))
  expect_true(all(na.omit(eDT$ID2) %in% IDs))
  expect_true(eDT[ID1 == ID2, .N] == 0)

  eDT <- edge_dist(
    copyDT,
    threshold = 50,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    fillNA = FALSE
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID1 %in% IDs))
  expect_true(all(eDT$ID2 %in% IDs))
  expect_true(eDT[ID1 == ID2, .N] == 0)

})



test_that('returnDist works', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')

  thresh <- 50
  withDist <- edge_dist(
    copyDT,
    threshold = thresh,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    fillNA = TRUE
  )

  woDist <- edge_dist(
    copyDT,
    threshold = thresh,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = FALSE,
    fillNA = TRUE
  )

  expect_equal(withDist[, .(ID1, ID2, timegroup)],
               woDist[, .(ID1, ID2, timegroup)])

  expect_equal(nrow(withDist),
               nrow(woDist))

  expect_equal(withDist[is.na(ID2)],
               withDist[is.na(distance)])

  expect_equal(withDist[!is.na(ID2)],
               withDist[!is.na(distance)])

  expect_lt(withDist[, max(distance, na.rm = TRUE)],
            thresh)


  withDistNoNA <- edge_dist(
    copyDT,
    threshold = thresh,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    fillNA = FALSE
  )

  expect_true(withDistNoNA[is.na(distance), .N] == 0)
  expect_true(withDistNoNA[is.na(ID2), .N] == 0)
  expect_lt(withDistNoNA[, max(distance, na.rm = TRUE)],
            thresh)

})


test_that('returns a data.table', {
  expect_s3_class(edge_dist(
    DT,
    threshold = 10,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = NULL
  ), 'data.table')
})



test_that('warns about splitBy column', {
  copyDT <- copy(DT)

  group_times(copyDT, 'datetime', '5 minutes')
  copyDT[, splitBy := as.IDate(datetime)]

  expect_warning(
    edge_dist(
      copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    ),
    'split_by'
  )
})
