context("General utilities")

# Check round_up function

testthat::test_that("round_up rounds 5 up", {
  testthat::expect_equal(tramlr::round_up(2.5), 3)
  testthat::expect_equal(tramlr::round_up(0.0005, 3), 0.001)
  testthat::expect_equal(tramlr::round_up(5000, -4), 10000)
})

# Check that get_separate_years is working

testthat::test_that("get_separate_years separates years correctly", {
  testthat::expect_equal(tramlr::get_separate_years("2019/20"), c("2019", "2020"))
  testthat::expect_equal(tramlr::get_separate_years("2012/13"), c("2012", "2013"))
  testthat::expect_equal(tramlr::get_separate_years("2020/21"), c("2020", "2021"))
})
