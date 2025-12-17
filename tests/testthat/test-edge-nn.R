# Test edge_nn
context('test edge_nn')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
coords <- c('X', 'Y')
id <- 'ID'
utm <- 32736
threshold <- 10
timegroup <- 'timegroup'

group_times(DT, 'datetime', '10 minutes')
get_geometry(DT, coords = coords, crs = utm)

test_that('DT is required', {
  expect_error(edge_nn(
    DT = NULL,
    id = id
  ),
  'DT must be provided')
})

test_that('ID and coords column names, threshold correctly provided',
          {
            expect_error(edge_nn(DT, id = NULL),
                         'id must')

            expect_error(
              edge_nn(
                DT,
                threshold = 10,
                id = id,
                coords = 'X',
                timegroup = timegroup
              ),
              'coords must be length 2',
              fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  # where ID field doesn't exist in DT
  expect_error(
    edge_nn(
      DT,
      id = 'potato',
      coords = coords,
      timegroup = timegroup
    ),
    'not present in input',
    fixed = FALSE
  )

  # where coords don't exist
  expect_error(
    edge_nn(
      DT,
      id = id,
      coords = c('potatoX', 'potatoY'),
      timegroup = timegroup
    ),
    'not present in input',
    fixed = FALSE
  )

  # where group fields doesn't exist
  expect_error(
    edge_nn(
      DT,
      id = id,
      coords = coords,
      splitBy = 'potato',
      timegroup = timegroup
    ),
    'not present in input',
    fixed = FALSE
  )

  # where timegroup field doesn't exist
  expect_error(
    edge_nn(
      DT,
      id = id,
      coords = coords,
      timegroup = 'potato'
    ),
    'not present in input',
    fixed = FALSE
  )
})


test_that('threshold correctly provided or error detected', {
  copyDT <- copy(DT)

  expect_error(edge_nn(DT, threshold = -10, timegroup = timegroup, id = id,
                       coords = coords),
               'threshold must be > 0')

  expect_error(edge_nn(DT, threshold = 0, timegroup = timegroup, id = id,
                       coords = coords),
               'threshold must be > 0')

  expect_error(edge_nn(DT, threshold = '0', timegroup = timegroup, id = id,
                       coords = coords),
               'threshold must be of class numeric')
})


test_that('coords are correctly provided or error detected', {
  expect_error(
    edge_nn(
      DT,
      id = id,
      timegroup = timegroup,
      coords = c('X', NULL)
    ),
    'coords must be length 2'
  )

  expect_error(
    edge_nn(
      DT,
      id = id,
      coords = c('X', 'ID'),
      timegroup = timegroup
    ),
    'coords must be of class numeric'
  )
})

test_that('warns if timegroup is a datetime or character',
          {
            # if datetime is a character
            copyDT <- copy(DT)
            expect_warning(
              edge_nn(
                copyDT,
                id = id,
                coords = coords,
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
                id = id,
                coords = coords,
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
                id = id,
                coords = coords,
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
    id = id,
    coords = coords,
    timegroup = timegroup
  ),
  'found duplicate id in a timegroup', fixed = FALSE)
})


test_that('returned IDs make sense', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID %in% IDs))
  expect_true(all(na.omit(eDT$NN) %in% IDs))
  expect_true(eDT[ID == NN, .N] == 0)

  # With threshold
  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
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
    id = id,
    coords = coords,
    timegroup = timegroup
  )

  expect_true(all(c('ID', 'NN') %in% colnames(eDT)))
  expect_false('distance' %in% colnames(eDT))


  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE
  )

  expect_true(all(c('ID', 'NN', 'distance') %in% colnames(eDT)))


  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE,
    threshold = threshold
  )

  expect_true(all(c('ID', 'NN', 'distance') %in% colnames(eDT)))

  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = FALSE,
    threshold = threshold
  )

  expect_true(all(c('ID', 'NN') %in% colnames(eDT)))
  expect_false('distance' %in% colnames(eDT))


})


test_that('distances returned are below threshold', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  threshold <- 1000
  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE,
    threshold = threshold
  )

  expect_equal(eDT[distance > threshold, .N], 0)
})

test_that('NAs exist in NN when threshold provided', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  threshold <- 1000
  eDT <- edge_nn(
    copyDT,
    id = id,
    coords = coords,
    timegroup = timegroup,
    threshold = threshold
  )

  expect_gt(eDT[is.na(NN), .N], 0)
})

test_that('returns a data.table', {
  expect_s3_class(edge_nn(
    DT,
    id = id,
    coords = coords,
    timegroup = timegroup
  ), 'data.table')
})



test_that('warns about splitBy column', {
  copyDT <- copy(DT)

  group_times(copyDT, 'datetime', '5 minutes')
  copyDT[, splitBy := as.IDate(datetime)]

  expect_warning(
    edge_nn(
      copyDT,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'split_by'
  )
})

test_that({'errors if timegroup is null'}, {
  expect_error(
    edge_nn(
      DT,
      threshold = NULL,
      id = id,
      coords = coords,
      timegroup = NULL
    )
  )
})
