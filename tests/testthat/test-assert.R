# Test assert family
context('test assert_*')

test <- data.table(X = LETTERS, Y = LETTERS,
                   ANGLE = runif(length(LETTERS), 0, 180))
units(test$ANGLE) <- 'degrees'


test_that('assert_not_null', {
  expect_error(assert_not_null(NULL))
})

test_that('assert_is_data_table', {
  expect_error(assert_is_data_table(data.frame()))
  expect_error(assert_is_data_table(data.table()))
})

test_that('assert_are_colnames', {
  expect_error(assert_are_colnames(test, 'Z'))
  expect_error(assert_are_colnames(test, c('X', 'Z')))
})

test_that('assert_col_inherits', {
  expect_error(assert_col_inherits(test, 'X', 'numeric'))
  expect_error(assert_col_inherits(test, c('X', 'Y'), 'numeric'))
})

test_that('assert_col_typeof', {
  expect_error(assert_col_typeof(test, 'X', 'units'))
  expect_null(assert_col_typeof(test, 'X', 'character'))
})

test_that('assert_inherits', {
  expect_error(assert_inherits(42, 'character'))
  expect_error(assert_inherits(TRUE, 'numeric'))
})


test_that('assert_col_radians', {
  expect_error(assert_col_radians(test, 'ANGLE',
                                  ', did you use direction_step?'))
})
test_that('assert_length', {
  expect_error(assert_length(1:2, 3L))
})
test_that('assert_relation', {
  expect_error(assert_relation(1, '>', 3))
})

test_that('assert_not_missing', {
  expect_error((function(x) assert_not_missing(x))())
})

test_that('assert_threshold', {
  expect_error(assert_threshold(0), '> 0')
  expect_error(assert_threshold(-1), '> 0')
  expect_silent(assert_threshold(1))
  expect_error(assert_threshold(units::as_units(-1, 'm')), '> units')
  expect_error(assert_threshold(units::as_units(1, 'degree'), 4326),
               'do not match')
  expect_silent(assert_threshold(units::as_units(1, 'm'), 4326))
  expect_silent(assert_threshold(units::as_units(1, 'm'), 32649))
  expect_silent(assert_threshold(NULL))
  expect_silent(assert_threshold(100, 4326))
  expect_error(assert_threshold(units::as_units(-1, 'm'), 32736))
})
