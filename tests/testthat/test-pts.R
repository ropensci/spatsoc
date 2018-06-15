# Test Points
context('test group_pts')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')

test_that('DT is required', {
  expect_error(group_pts(
    DT = NULL,
    threshold = 10,
    idField = 'ID'
  ),
  'input DT required')
})

test_that('ID and coordFields column names, threshold must be provided (and correctly)',
          {
            expect_error(group_pts(DT, threshold = 10, idField = NULL),
                         'ID field required')

            expect_error(group_pts(DT, threshold = NULL, idField = 'ID'),
                         'threshold required')

            expect_error(
              group_pts(
                DT,
                threshold = 10,
                idField = 'ID',
                coordFields = 'X'
              ),
              'coordFields requires a vector',
              fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  # where ID field doesn't exist in DT
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'potato',
      coordFields = c('X', 'Y')
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where coordFields don't exist
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('potatoX', 'potatoY')
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where group fields doesn't exist
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      groupFields = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  # where timegroup field doesn't exist
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      timegroup = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})


test_that('threshold correctly provided or error detected', {
  expect_silent(group_pts(
    DT,
    threshold = 10,
    idField = 'ID',
    coordFields = c('X', 'Y')
  ))

  expect_error(group_pts(DT, threshold = -10, idField = 'ID'),
               'threshold must be greater than 0')

  expect_error(group_pts(DT, threshold = 0, idField = 'ID'),
               'threshold must be greater than 0')


})


test_that('coordFields are correctly provided or error detected', {
  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', NULL)
    ),
    'coordFields requires a vector'
  )

  expect_error(
    group_pts(
      DT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', 'ID')
    ),
    'coordFields must be numeric'
  )
})

test_that('two column DT returned if timegroup, group fields not provided', {
  expect_equal(ncol(group_pts(
    DT,
    threshold = 10,
    idField = 'ID',
    coordFields = c('X', 'Y')
  )),
  2)
})

test_that('warns if timegroup is a date/time or character instead of output from group_times',
          {
            copyDT <- copy(DT)

            # if datetime is a character
            expect_warning(
              group_pts(
                copyDT,
                threshold = 10,
                idField = 'ID',
                coordFields = c('X', 'Y'),
                timegroup = 'datetime'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )

            # if datetime is a POSIXct
            copyDT[, datetime := as.POSIXct(datetime)]
            expect_warning(
              group_pts(
                copyDT,
                threshold = 10,
                idField = 'ID',
                coordFields = c('X', 'Y'),
                timegroup = 'datetime'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )

            # if datetime is an IDate
            copyDT[, idate := as.IDate(datetime)]
            expect_warning(
              group_pts(
                copyDT,
                threshold = 10,
                idField = 'ID',
                coordFields = c('X', 'Y'),
                timegroup = 'idate'
              ),
              'timegroup provided is a',
              fixed = FALSE
            )
          })


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1]
  expect_warning(
    group_pts(
      copyDT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', 'Y')
    ),
    'group column will be overwritten'
  )
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, timeField = 'datetime', threshold = '5 minutes')

  expect_false('withinGroup' %in% colnames(
    group_pts(
      copyDT,
      threshold = 10,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      timegroup = 'timegroup'
    )
  ))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, timeField = 'datetime', threshold = '5 minutes')

  expect_equal(nrow(copyDT),
               nrow(
                 group_pts(
                   copyDT,
                   threshold = 10,
                   idField = 'ID',
                   coordFields = c('X', 'Y'),
                   timegroup = 'timegroup'
                 )
               ))
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, timeField = 'datetime', threshold = '5 minutes')

  expect_equal(ncol(copyDT) + 1,
               ncol(
                 group_pts(
                   copyDT,
                   threshold = 10,
                   idField = 'ID',
                   coordFields = c('X', 'Y'),
                   timegroup = 'timegroup'
                 )
               ))
})

test_that('group column is added to result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, timeField = 'datetime', threshold = '5 minutes')
  expect_true('group' %in%
                colnames(
                  group_pts(
                    copyDT,
                    threshold = 10,
                    idField = 'ID',
                    coordFields = c('X', 'Y'),
                    timegroup = 'timegroup'
                  )
                ))
})
