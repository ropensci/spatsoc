# Test diff_rad
context('test diff_rad')

library(units)

pt_01 <- as_units(0.1, 'rad')
pt_02 <- as_units(0.2, 'rad')
pi_rad <- as_units(pi, 'rad')


expect_equal(diff_rad(pt_01, pt_02, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 + 2 * pi_rad, TRUE), pt_01)
expect_equal(diff_rad(pt_01, pt_02 - 2 * pi_rad, TRUE), pt_01)
expect_equal(diff_rad(pt_01 + 2 * pi_rad, pt_02, TRUE), pt_01)
expect_equal(diff_rad(pt_01 - 2 * pi_rad, pt_02, TRUE), pt_01)
expect_equal(diff_rad(pt_02, pt_01, TRUE), -pt_01)
expect_equal(diff_rad(pt_02 + 2 * pi_rad, pt_01, TRUE), -pt_01)
expect_equal(diff_rad(pt_02 - 2 * pi_rad, pt_01, TRUE), -pt_01)
expect_equal(diff_rad(pt_02, pt_01 + 2 * pi_rad, TRUE), -pt_01)
expect_equal(diff_rad(pt_02, pt_01 - 2 * pi_rad, TRUE), -pt_01)
