library(devtools)
library(testthat)
library(ESM262PackageAssignment)

test_that("growth_rate_works",

          #make sure we get zero when constant term and temp are zero
          {expect_that(growth_rate(t = 0, a = 0), equals(0))
            #make sure output type is double
            expect_type(growth_rate(15), "double")
            #make sure growth rate is above -100% when temp is in reasonable range
            expect_true(min(growth_rate(0:30)) > -100)
          })
