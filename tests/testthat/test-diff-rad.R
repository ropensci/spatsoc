# Test diff_rad
context('test diff_rad')

library(units)

pt_01 <- as_units(0.1, 'rad')
pt_02 <- as_units(0.2, 'rad')
pi_rad <- as_units(pi, 'rad')

# Adapted from: https://gist.github.com/bradphelan/7fe21ad8ebfcb43696b8
expect_equal(diff_rad(pt_01, pt_02, TRUE, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 + 2 * pi_rad, TRUE, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 - 2 * pi_rad, TRUE, TRUE), pt_01)
expect_equal(diff_rad(pt_01 + 2 * pi_rad, pt_02, TRUE, TRUE), pt_01)
expect_equal(diff_rad(pt_01 - 2 * pi_rad, pt_02, TRUE, TRUE), pt_01)
expect_equal(diff_rad(pt_02, pt_01, TRUE, TRUE), -pt_01)
expect_equal(diff_rad(pt_02 + 2 * pi_rad, pt_01, TRUE, TRUE), -pt_01)
expect_equal(diff_rad(pt_02 - 2 * pi_rad, pt_01, TRUE, TRUE), -pt_01)
expect_equal(diff_rad(pt_02, pt_01 + 2 * pi_rad, TRUE, TRUE), -pt_01)
expect_equal(diff_rad(pt_02, pt_01 - 2 * pi_rad, TRUE, TRUE), -pt_01)

expect_equal(diff_rad(pt_01, pt_02, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 + 2 * pi_rad, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 - 2 * pi_rad, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_01 + 2 * pi_rad, pt_02, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_01 - 2 * pi_rad, pt_02, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_02, pt_01, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_02 + 2 * pi_rad, pt_01, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_02 - 2 * pi_rad, pt_01, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_02, pt_01 + 2 * pi_rad, FALSE, TRUE), pt_01)
expect_equal(diff_rad(pt_02, pt_01 - 2 * pi_rad, FALSE, TRUE), pt_01)

expect_error(diff_rad(pt_01, 0.2), 'rad')
expect_error(diff_rad(0.1, pt_02), 'rad')
