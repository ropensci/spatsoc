# Test Random
context('test randomizations')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

group_times(DT, datetime = 'datetime', threshold = '1 hour')
group_pts(DT, threshold = 100, id = 'ID', timegroup = 'timegroup',
        coords = c('X', 'Y'))

test_that('DT, type, id, datetime are required', {
  expect_error(randomizations(DT = NULL),
               'input DT required')

  expect_error(randomizations(DT = DT,
                              type = NULL),
               'type of randomization', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = NULL),
               'id field required')

  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'ID',
                              datetime = NULL),
               'datetime field required')

})

test_that('type must be one of options', {
  expect_error(randomizations(DT = DT,
                              type = 'potato'),
               'type of randomization must be one of', fixed = FALSE)
})

test_that('fields provided must be in DT', {
  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'potato',
                              datetime = 'datetime'),
               'field(s) provided are not present', fixed = TRUE)

  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'ID',
                              datetime = 'potato'),
               'field(s) provided are not present', fixed = TRUE)

  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'ID',
                              datetime = 'datetime',
                              splitBy = 'potato'),
               'field(s) provided are not present', fixed = TRUE)
})

test_that('iterations is NULL or correctly provided', {
  copyDT <- copy(DT)
  expect_warning(randomizations(DT = copyDT,
                              type = 'step',
                              id = 'ID',
                              datetime = 'datetime',
                              iterations = NULL),
               'iterations is not', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'ID',
                              datetime = 'datetime',
                              iterations = 'potato'),
               'either provide a numeric for iterations or NULL', fixed = FALSE)
})

test_that('dateFormatted or not depending on randomization type', {
  copyDT <- copy(DT)
  expect_warning(randomizations(DT = copyDT,
                                type = 'step',
                                id = 'ID',
                                datetime = 'datetime',
                                iterations = 1),
                 'datetime provided is POSIXct', fixed = FALSE)

  DT[, numDate := 1]
  expect_error(randomizations(DT = DT,
                              type = 'daily',
                              id = 'ID',
                              datetime = 'numDate',
                              iterations = 1),
                 'datetime must be POSIXct', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'trajectory',
                              id = 'ID',
                              datetime = 'numDate',
                              iterations = 1),
                 'datetime must be POSIXct', fixed = FALSE)
})


test_that('jul column found and warn overwrite', {
  copyDT <- copy(DT)
  copyDT[, jul := 1][, datetime := as.POSIXct(datetime)]
  expect_message(randomizations(DT = copyDT,
                                type = 'daily',
                                id = 'ID',
                                datetime = 'datetime',
                                iterations = 1),
                 'column jul found in DT', fixed = FALSE)

  copyDT <- copy(DT)
  copyDT[, jul := 1]
  expect_message(randomizations(DT = copyDT,
                                type = 'trajectory',
                                id = 'ID',
                                datetime = 'datetime',
                                iterations = 1),
                 'column jul found in DT', fixed = FALSE)
})


test_that('step randomization returns as expected', {
  copyDT <- copy(DT)
  expect_equal(
    randomizations(
      DT = copyDT,
      type = 'step',
      id = 'ID',
      iterations = 1,
      datetime = 'timegroup'
    )[, uniqueN(randomID), by = timegroup],
    DT[, uniqueN(ID), by = timegroup])

  copyDT <- copy(DT)
  expect_equal(
    nrow(randomizations(
      DT = copyDT,
      type = 'step',
      id = 'ID',
      iterations = 3,
      datetime = 'timegroup'
    )),
    nrow(DT) * (3 + 1))

  copyDT <- copy(DT)
  copyDT[, population := 1][1:50, population := 2]
  expect_equal(
    randomizations(
      DT = copyDT,
      type = 'step',
      id = 'ID',
      iterations = 1,
      datetime = 'timegroup',
      splitBy = 'population'
    )[, uniqueN(randomID), by = timegroup],
    copyDT[, uniqueN(ID), by = timegroup])
})


test_that('daily randomization returns as expected', {
  copyDT <- copy(DT)
  expect_equal(
    randomizations(
      DT = copyDT,
      type = 'daily',
      id = 'ID',
      iterations = 1,
      datetime = 'datetime'
    )[, .(N = uniqueN(randomID)),
      by = .(jul, ID)][, max(N)],
    1)

})

test_that('trajectory randomization returns as expected', {
  copyDT <- copy(DT)
  expect_equal(
    randomizations(
      DT = copyDT,
      type = 'trajectory',
      id = 'ID',
      iterations = 1,
      datetime = 'datetime'
    )[, uniqueN(randomJul), by = .(ID, jul)][, max(V1)],
    1)

  copyDT <- copy(DT)
  expect_equal(
    nrow(randomizations(
      DT = copyDT,
      type = 'trajectory',
      id = 'ID',
      iterations = 3,
      datetime = 'datetime'
    )),
    nrow(DT) * (3 + 1))
})

test_that('non uniques are found (iterations > 1)', {
  copyDT <- copy(DT)
  copyDT[2, (colnames(DT)) := copyDT[1, .SD]]

  expect_warning(randomizations(DT = copyDT,
                                type = 'daily',
                                id = 'ID',
                                datetime = 'datetime',
                                iterations = 2),
                 'found none unique rows of id, datetime',
                 fixed = FALSE)
})

test_that('iterations > 1 returns expected columns', {
  expect_true('observed' %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_true('iteration' %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_true('rowID' %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  # if step, randomID
  expect_true('randomID' %in% colnames(
    randomizations(
      DT = DT,
      type = 'step',
      id = 'ID',
      datetime = 'timegroup',
      iterations = 2
    )
  ))

  # if daily, randomID and jul
  expect_true('randomID' %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_true('jul' %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  # if trajectory, randomJul and randomdatetime and jul
  expect_true('randomJul' %in% colnames(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_true('randomdatetime' %in% colnames(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      datetime = 'datetime',
      iterations = 2
    )
  ))

})


##
test_that('iterations = 1 detects columns already present', {
  # step, randomID
  copyDT <- copy(DT)
  copyDT[, randomID := 'a']

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'step',
      id = 'ID',
      datetime = 'timegroup',
      iterations = 1
    ),
    'column randomID found in DT',
    fixed = FALSE
  )

  # daily, randomID
  copyDT <- copy(DT)
  copyDT[, randomID := 'a']

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 1
    ),
    'column randomID found in DT',
    fixed = FALSE
  )

  # daily, jul
  copyDT <- copy(DT)
  copyDT[, jul := 1]

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'daily',
      id = 'ID',
      datetime = 'datetime',
      iterations = 1
    ),
    'column jul found in DT',
    fixed = FALSE
  )

  # trajectory, jul
  copyDT <- copy(DT)
  copyDT[, jul := 1]

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'trajectory',
      id = 'ID',
      datetime = 'datetime',
      iterations = 1
    ),
    'column jul found in DT',
    fixed = FALSE
  )

  # trajectory, randomJul
  copyDT <- copy(DT)
  copyDT[, randomJul := 1]

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'trajectory',
      id = 'ID',
      datetime = 'datetime',
      iterations = 1
    ),
    'column randomJul found in DT',
    fixed = FALSE
  )

  # trajectory, randomdatetime
  copyDT <- copy(DT)
  copyDT[, randomdatetime := 1]

  expect_message(
    randomizations(
      DT = copyDT,
      type = 'trajectory',
      id = 'ID',
      datetime = 'datetime',
      iterations = 1
    ),
    'column randomdatetime found in DT',
    fixed = FALSE
  )
})
