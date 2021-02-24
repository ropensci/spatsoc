context("test-edge-nn")

library(spatsoc)

DT <- fread('../testdata/DT.csv')

test_that('DT is required', {
  expect_error(edge_nn(
    DT = NULL,
    id = 'ID'
  ),
  'input DT required')
})

test_that('ID and coords column names, threshold correctly provided',
          {
            expect_error(edge_nn(DT, id = NULL),
                         'ID field required')

            expect_error(
              edge_nn(
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
    edge_nn(
      DT,
      id = 'potato',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where coords don't exist
  expect_error(
    edge_nn(
      DT,
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      timegroup = 'timegroup'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where group fields doesn't exist
  expect_error(
    edge_nn(
      DT,
      id = 'ID',
      coords = c('X', 'Y'),
      splitBy = 'potato',
      timegroup = 'timegroup'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where timegroup field doesn't exist
  expect_error(
    edge_nn(
      DT,
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

  expect_error(edge_nn(DT, threshold = -10, id = 'ID'),
               'threshold must be greater than 0')

  expect_error(edge_nn(DT, threshold = 0, id = 'ID'),
               'threshold must be greater than 0')

  expect_error(edge_nn(DT, threshold = '0', id = 'ID'),
               'threshold must be numeric')
})


test_that('coords are correctly provided or error detected', {
  expect_error(
    edge_nn(
      DT,
      id = 'ID',
      coords = c('X', NULL)
    ),
    'coords requires a vector'
  )

  expect_error(
    edge_nn(
      DT,
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
              edge_nn(
                copyDT,
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
              edge_nn(
                copyDT,
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
              edge_nn(
                copyDT,
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
  expect_warning(edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  ),
  'found duplicate id in a timegroup', fixed = FALSE)
})


test_that('returned IDs make sense', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID %in% IDs))
  expect_true(all(na.omit(eDT$NN) %in% IDs))
  expect_true(eDT[ID == NN, .N] == 0)

  # With threshold
  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    threshold = 100
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID %in% IDs))
  expect_true(all(na.omit(eDT$NN) %in% IDs))
  expect_true(eDT[ID == NN, .N] == 0)


})






test_that('returned columns match', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  )

  expect_true(all(c('ID', 'NN') %in% colnames(eDT)))
  expect_false('distance' %in% colnames(eDT))


  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE
  )

  expect_true(all(c('ID', 'NN', 'distance') %in% colnames(eDT)))


  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    threshold = 1000
  )

  expect_true(all(c('ID', 'NN', 'distance') %in% colnames(eDT)))

  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = FALSE,
    threshold = 1000
  )

  expect_true(all(c('ID', 'NN') %in% colnames(eDT)))
  expect_false('distance' %in% colnames(eDT))


})


test_that('distances returned are below threshold', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  thresh <- 1000
  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    threshold = thresh
  )

  expect_equal(eDT[distance > thresh, .N], 0)
})

test_that('NAs exist in NN when threshold provided', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  thresh <- 1000
  eDT <- edge_nn(
    copyDT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    threshold = thresh
  )

  expect_gt(eDT[is.na(NN), .N], 0)
})

test_that('returns a data.table', {
  expect_s3_class(edge_nn(
    DT,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = NULL
  ), 'data.table')
})
