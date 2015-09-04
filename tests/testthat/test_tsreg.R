library(Rgrowth)
context("output of tsreg")

data(ndmi)

test_that("ndmi inherits from zoo", {
  expect_is(ndmi, "zoo")
})


test_that("time index of ndmi is properly formatted", {
  dates <- time(ndmi)
  year <- as.numeric(format(dates, format = "%Y"))
  jd <- as.numeric(format(dates, format = "%Y%j"))
  
  expect_true(year > 1000 && year < 3000)
  expect_true(jd > 1000000 && jd < 3000000)
})
