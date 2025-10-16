# Test assert family
context('test assert_*')

expect_error(assert_not_null(NULL))
expect_error(assert_is_data_table(data.frame()))
expect_error(assert_is_data_table(data.table()))

test <- data.table(X = LETTERS, Y = LETTERS, ANGLE = runif(length(LETTERS), 0, 180))
units(test$ANGLE) <- 'degrees'

expect_error(assert_are_colnames(test, 'Z'))
expect_error(assert_are_colnames(test, c('X', 'Z')))

expect_error(assert_col_inherits(test, 'X', 'numeric'))
expect_error(assert_col_inherits(test, c('X', 'Y'), 'numeric'))

expect_error(assert_col_typeof(test, 'X', 'units'))

expect_error(assert_inherits(42, 'character'))
expect_error(assert_inherits(TRUE, 'numeric'))

expect_error(assert_col_radians(test, 'ANGLE', ', did you use direction_step?'))
expect_error(assert_length(1:2, 3L))

expect_error(assert_relation(1, '>', 3))

expect_error((function(x) assert_not_missing(x))())
