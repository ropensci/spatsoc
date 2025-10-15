# Test assert family
context('test assert_*')

expect_error(assert_not_null(NULL))
expect_error(assert_is_data_table(data.frame()))
expect_error(assert_is_data_table(data.table()))


test <- data.table(X = LETTERS, Y = LETTERS)
expect_error(assert_are_colnames(test, 'Z'))
expect_error(assert_are_colnames(test, c('X', 'Z')))

expect_error(assert_inherits(test, 'X', 'numeric'))
expect_error(assert_inherits(test, c('X', 'Y'), 'numeric'))

expect_error(assert_length(1:2, 3L))
