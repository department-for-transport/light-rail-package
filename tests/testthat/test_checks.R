context("Error checking")

# Checking string

testthat::test_that("Input is string",{
  testthat::expect_error(tramlr::check_string(2019), "Make sure it is surrounded by quotes, and that it is of length 1")
  testthat::expect_error(tramlr::check_string(c("a", "b")), "Make sure it is surrounded by quotes, and that it is of length 1")
  testthat::expect_error(tramlr::check_string(TRUE), "Make sure it is surrounded by quotes, and that it is of length 1")
})

# Checking path

testthat::test_that("Path leads to existing directory or file",{
  testthat::expect_error(tramlr::check_path("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/6. Valida"),
               "The directory or file given by this path does not exist")
  testthat::expect_equal(tramlr::check_path("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/6. Validation"),
               "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/6. Validation")
  testthat::expect_equal(tramlr::check_path("G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Survey response emails.xlsx"),
               "G:/AFP/RLTDAll/STS/003 BLT/003 LIGHT RAIL/0001 Data/2020/3. Survey/Survey response emails.xlsx")
})

# Checking financial year

testthat::test_that("Financial year has correct number of forward slashes",{
  testthat::expect_error(tramlr::check_financial_year("201920"), "does not include a forward slash")
  testthat::expect_error(tramlr::check_financial_year("2019//20"), "contains more than one forward slash")
  testthat::expect_error(tramlr::check_financial_year("20/19/20"), "contains more than one forward slash")
})

testthat::test_that("Financial year has correct number of characters",{
  testthat::expect_error(tramlr::check_financial_year("2019/320"), "It should have 7 characters and be a string")
  testthat::expect_error(tramlr::check_financial_year("2019/0"), "It should have 7 characters and be a string")
  testthat::expect_error(tramlr::check_financial_year("/"), "It should have 7 characters and be a string")
})

testthat::test_that("Financial year has forward slash in correct place",{
  testthat::expect_error(tramlr::check_financial_year("201/920"), "There should be 4 before and 2 after")
  testthat::expect_error(tramlr::check_financial_year("20192/0"), "There should be 4 before and 2 after")
  testthat::expect_error(tramlr::check_financial_year("/201920"), "There should be 4 before and 2 after")
})

testthat::test_that("Financial year is made up of numbers",{
  testthat::expect_error(tramlr::check_financial_year("2019/2o"), "must contain ONLY numbers and a forward slash")
  testthat::expect_error(tramlr::check_financial_year("20ab/20"), "must contain ONLY numbers and a forward slash")
  testthat::expect_error(tramlr::check_financial_year("201l/20"), "must contain ONLY numbers and a forward slash")
})

testthat::test_that("Financial year is made up of consecutive years",{
  testthat::expect_error(tramlr::check_financial_year("2019/21"), "MUST contain two consecutive years")
  testthat::expect_error(tramlr::check_financial_year("2000/20"), "MUST contain two consecutive years")
  testthat::expect_error(tramlr::check_financial_year("1990/18"), "MUST contain two consecutive years")
})

# Checking year gaps

testthat::test_that("Year gaps errors", {
  testthat::expect_error(tramlr::check_year_gap("6"), "The input must be a number.")
  testthat::expect_error(tramlr::check_year_gap(4.5), "The input must be a whole number.")
  testthat::expect_error(tramlr::check_year_gap(13), "The input must be between 1 and 12.")
  testthat::expect_error(tramlr::check_year_gap(0), "The input must be between 1 and 12.")
})

testthat::test_that("Year gaps correct", {
  testthat::expect_equal(tramlr::check_year_gap(12), 12)
  testthat::expect_equal(tramlr::check_year_gap(1), 1)
  testthat::expect_equal(tramlr::check_year_gap(6), 6)
})
