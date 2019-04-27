# Test Random
context('test randomizations')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

group_times(DT, datetime = 'datetime', threshold = '1 hour')
group_pts(
  DT,
  threshold = 100,
  id = 'ID',
  timegroup = 'timegroup',
  coords = c('X', 'Y')
)

test_that('DT, type, id, datetime, (group) are required', {
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

  # Group required for daily and step
  expect_error(randomizations(DT = DT,
                              type = 'step',
                              id = 'ID',
                              iterations = 1,
                              group = NULL,
                              datetime = 'timegroup'),
               'group field must be provided if type is "step" or "daily"')

  expect_error(randomizations(DT = DT,
                              type = 'daily',
                              id = 'ID',
                              group = NULL,
                              iterations = 1,
                              datetime = 'datetime'),
               'group field must be provided if type is "step" or "daily"')

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
                              group = 'group',
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
  expect_warning(randomizations(DT = DT,
                                type = 'step',
                                id = 'ID',
                                group = 'group',
                                datetime = 'datetime',
                                iterations = 1),
                 'datetime provided is POSIXct', fixed = FALSE)

  DT[, numDate := 1]
  expect_error(randomizations(DT = DT,
                              type = 'daily',
                              id = 'ID',
                              group = 'group',
                              datetime = 'numDate',
                              iterations = 3),
                 'datetime must be POSIXct', fixed = FALSE)

  expect_error(randomizations(DT = DT,
                              type = 'trajectory',
                              id = 'ID',
                              coords = c('X', 'Y'),
                              datetime = 'numDate',
                              iterations = 3),
                 'datetime must be POSIXct', fixed = FALSE)
})


test_that('step randomization returns as expected', {
  # N unique randomIDs same as N unique IDs in each timegroup
  expect_equal(
    randomizations(
      DT = DT,
      type = 'step',
      id = 'ID',
      group = 'group',
      iterations = 1,
      datetime = 'timegroup'
    )[, uniqueN(randomID), by = timegroup],
    DT[, uniqueN(ID), by = timegroup])

  # N rows in output is (nrows * iterations + 1)
  expect_equal(nrow(
    randomizations(
      DT = DT,
      type = 'step',
      id = 'ID',
      group = 'group',
      iterations = 3,
      datetime = 'timegroup'
    )
  ),
  nrow(DT) * (3 + 1))

  copyDT <- copy(DT)[, population := 1][1:50, population := 2]
  expect_equal(
    randomizations(
      DT = copyDT,
      type = 'step',
      id = 'ID',
      group = 'group',
      iterations = 1,
      datetime = 'timegroup',
      splitBy = 'population'
    )[, uniqueN(randomID), by = timegroup],
    copyDT[, uniqueN(ID), by = timegroup])
})


test_that('daily randomization returns as expected', {
  expect_equal(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      group = 'group',
      iterations = 1,
      datetime = 'datetime'
    )[, .(N = uniqueN(randomID)),
      by = .(jul, ID, iteration)][, max(N)],
    1)
})

test_that('trajectory randomization returns as expected', {
  expect_equal(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      coords = c('X', 'Y'),
      iterations = 1,
      datetime = 'datetime'
    )[, uniqueN(randomJul), by = .(ID, jul, iteration)][, max(V1)],
    1)


  expect_equal(
    nrow(randomizations(
      DT = DT,
      type = 'trajectory',
      coords = c('X', 'Y'),
      id = 'ID',
      iterations = 3,
      datetime = 'datetime'
    )),
    nrow(DT) * (3 + 1))

  expect_true(all(c('X', 'Y', 'randomdatetime') %in%
                    colnames(
                      randomizations(
                        DT = DT,
                        type = 'trajectory',
                        coords = c('X', 'Y'),
                        id = 'ID',
                        iterations = 3,
                        datetime = 'datetime'
                      )
                    )))

})

test_that('non uniques are found', {
  copyDT <- copy(DT)
  copyDT[2, (colnames(DT)) := copyDT[1, .SD]]

  expect_warning(randomizations(DT = copyDT,
                                type = 'daily',
                                id = 'ID',
                                group = 'group',
                                datetime = 'datetime',
                                iterations = 2),
                 'found non-unique rows of id, datetime',
                 fixed = FALSE)
})

test_that('randomization returns expected columns', {
  expect_true(all(c('observed', 'iteration')
                  %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      group = 'group',
      datetime = 'datetime',
      iterations = 2
    )
  )))

  # if step, randomID
  expect_true('randomID' %in% colnames(
    randomizations(
      DT = DT,
      type = 'step',
      id = 'ID',
      group = 'group',
      datetime = 'timegroup',
      iterations = 2
    )
  ))

  # if daily, randomID and jul
  expect_true(all(c('randomID', 'jul') %in% colnames(
    randomizations(
      DT = DT,
      type = 'daily',
      id = 'ID',
      group = 'group',
      datetime = 'datetime',
      iterations = 2
    )
  )))

  # if trajectory, randomJul and randomdatetime and jul
  expect_true('randomJul' %in% colnames(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      coords = c('X', 'Y'),
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_true('randomdatetime' %in% colnames(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      coords = c('X', 'Y'),
      datetime = 'datetime',
      iterations = 2
    )
  ))

  expect_error(
    randomizations(
      DT = DT,
      type = 'trajectory',
      id = 'ID',
      coords = NULL,
      datetime = 'datetime',
      iterations = 2),
      'coords must be provided if type is "trajectory"'
    )
})





test_that('randomization is silent when an individual only has one row', {
  ## Fixed here: 6092ad8d055ff269d39dd481a80f27069c790630

  # ** Fake an individual with 1 relocation**
  copyDT <- copy(DT)[1, ID := 'Z']

  # Temporal grouping
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  # Spatial grouping with timegroup
  group_pts(
    copyDT,
    threshold = 5,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup'
  )

  expect_silent(
    randomizations(
      copyDT,
      type = 'step',
      id = 'ID',
      group = 'group',
      datetime = 'timegroup',
      splitBy = 'yr',
      iterations = 2
    )
  )

  expect_silent(
    randomizations(
      copyDT,
      type = 'daily',
      id = 'ID',
      group = 'group',
      datetime = 'datetime',
      splitBy = 'yr',
      iterations = 2
    )
  )

  expect_silent(
    randomizations(
      copyDT,
      type = 'trajectory',
      id = 'ID',
      group = NULL,
      coords = c('X', 'Y'),
      datetime = 'datetime',
      splitBy = 'yr',
      iterations = 2
    )
  )

})



test_that('n individuals consistent across years', {
  DT[1, ID := 'Z']

  ## Step
  randStep <- randomizations(
    DT,
    type = 'step',
    id = 'ID',
    group = 'group',
    splitBy = 'yr',
    datetime = 'timegroup',
    iterations = 20
  )

  uID <- randStep[, .(nID = .N), by = .(randomID, yr, iteration)]

  expect_equal(
    uID[, sd(nID), by = .(randomID, yr)]$V1,
    rep(0, randStep[, uniqueN(randomID)])
  )

  ## Daily
  randDaily <- randomizations(
    DT,
    type = 'daily',
    id = 'ID',
    group = 'group',
    splitBy = 'yr',
    datetime = 'datetime',
    iterations = 20
  )
  uJul <- randDaily[, .(nDay = uniqueN(jul)), by = .(randomID, yr, iteration)]

  expect_equal(
    uJul[, sd(nDay), by = .(randomID, yr)]$V1,
    rep(0, nrow(randDaily[, .N, .(randomID, yr)]))
  )

})
