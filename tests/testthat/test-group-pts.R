# Test group_pts
context('test group_pts')

library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = 'datetime', threshold = '20 minutes')

test_that('DT is required', {
  expect_error(group_pts(
    DT = NULL,
    threshold = 10,
    id = 'ID'
  ),
  'DT must be provided')
})

test_that('ID, coord column names, time, threshold provided correctly',
          {
            expect_error(group_pts(DT, threshold = 10, id = NULL),
                         'id must be')

            expect_error(group_pts(DT, threshold = NULL, id = 'ID'),
                         'threshold must be')

            expect_error(group_pts(DT, threshold = 10, id = 'ID',
                                   coords = c('X', 'Y')),
                         'timegroup must be')

            expect_error(
              group_pts(
                DT,
                threshold = 10,
                id = 'ID',
                coords = 'X',
                timegroup = 'timegroup'
              ),
              'coords must be length 2',
              fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  # where ID field doesn't exist in DT
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      id = 'potato',
      coords = c('X', 'Y'),
      timegroup = NULL
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where coords don't exist
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      timegroup = NULL
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where group fields doesn't exist
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = NULL,
      splitBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where timegroup field doesn't exist
  expect_error(
    group_pts(
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

  expect_error(group_pts(DT, threshold = -10, id = 'ID'),
               'threshold must be > 0')

  expect_error(group_pts(DT, threshold = 0, id = 'ID'),
               'threshold must be > 0')

  expect_error(group_pts(DT, threshold = '0', id = 'ID'),
               'threshold must be of class numeric')
})


test_that('coords are correctly provided or error detected', {
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', NULL),
      timegroup = 'timegroup'
    ),
    'coords must be length 2'
  )

  expect_error(
    group_pts(
      DT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'ID'),
      timegroup = NULL
    ),
    'coords must be of class numeric'
  )
})

test_that('DT returned if timegroup, group fields not provided', {
  copyDT <- copy(DT)
  expect_equal(ncol(copyDT) + 1,
               ncol(group_pts(
                 copyDT,
                 threshold = 10,
                 id = 'ID',
                 coords = c('X', 'Y'),
                 timegroup = 'timegroup'
               )))

  # warns if > 1 ID row

  # same but with timegroup

  # and with splitBy
})

test_that('warns if timegroup is a datetime or character',
          {
            # if datetime is a character
            copyDT <- copy(DT)
            expect_warning(
              group_pts(
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
              group_pts(
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
              group_pts(
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


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1]
  expect_message(
    group_pts(
      copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    ),
    'group column will be overwritten'
  )
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_false('withinGroup' %in% colnames(
    group_pts(
      copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    )
  ))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_equal(nrow(copyDT),
               nrow(
                 group_pts(
                   copyDT,
                   threshold = 10,
                   id = 'ID',
                   coords = c('X', 'Y'),
                   timegroup = 'timegroup'
                 )
               ))
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_equal(ncol(copyDT) + 1,
               ncol(
                 group_pts(
                   copyDT,
                   threshold = 10,
                   id = 'ID',
                   coords = c('X', 'Y'),
                   timegroup = 'timegroup'
                 )
               ))
})

test_that('group column is added to result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')
  expect_true('group' %in%
                colnames(
                  group_pts(
                    copyDT,
                    threshold = 10,
                    id = 'ID',
                    coords = c('X', 'Y'),
                    timegroup = 'timegroup'
                  )
                ))
})

test_that('duplicate IDs in a timegroup detected', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '8 hours')
  expect_warning(group_pts(
                    copyDT,
                    threshold = 10,
                    id = 'ID',
                    coords = c('X', 'Y'),
                    timegroup = 'timegroup'
                  ),
                 'found duplicate id in a timegroup', fixed = FALSE)
})



test_that('returns a data.table', {
  expect_s3_class(group_pts(
    DT,
    threshold = 10,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  ), 'data.table')
})



test_that('splitBy argument doesnt use splitBy column', {
  copyDT <- copy(DT)

  copyDT[, splitBy := sample(seq.int(5), .N, TRUE)]

  expect_true(
    group_pts(
      copyDT,
      threshold = 10,
      id = 'ID',
      coords = c('X', 'Y'),
      timegroup = 'timegroup'
    )[, uniqueN(splitBy), group][V1 > 1, .N != 0]
  )

})
