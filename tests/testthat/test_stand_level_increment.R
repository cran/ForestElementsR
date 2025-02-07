
library(ForestElementsR)


test_that("increment calulation is correct" ,{

   # Normal application
   age        <- seq(20, 70, 5)
   vol_remain <- c(65, 118, 175, 233, 293, 355, 416, 476, 534, 589, 642)
   vol_remove <- c(16,  29,  35,  39,  39,  39,  38,  37,  36,  35,  34)
   inc_exp <- c(NA, 16.4, 18.4, 19.4, 19.8, 20.2, 19.8, 19.4, 18.8, 18.0, 17.4)

   inc_clc <- stand_level_increment(age, vol_remain, vol_remove)

   expect_equal(inc_clc, inc_exp)

   # Data from only one point in time must result in a single NA
   age        <- 20
   vol_remain <- 65
   vol_remove <- 16

   expect_true(is.na(stand_level_increment(age, vol_remain, vol_remove)))
})


test_that("badly dimensioned inputs trigger an error", {

  time <- c(1, 2, 3)
  rmn  <- c(1, 2, 3, 4)
  rmv  <- c(1, 2, 3, 4)

  expect_error(stand_level_increment(time, rmn, rmv))

  time <- c(1, 2, 3)
  rmn  <- c(1, 2)
  rmv  <- c(1, 2, 3)

  expect_error(stand_level_increment(time, rmn, rmv))

  time <- c(1, 2, 3)
  rmn  <- c(1, 2, 3)
  rmv  <- c(1, 2, 3, 4)

  expect_error(stand_level_increment(time, rmn, rmv))

  time <- c(1, 2, 3)
  rmn  <- c(1, 2, 3, 4)
  rmv  <- c(1, 2, 3, 4, 5)

  expect_error(stand_level_increment(time, rmn, rmv))
})


test_that("the function terminates when the input vectors contain NAs", {

  age        <- seq(20, 70, 5)
  vol_remain <- c(65, 118, 175, 233, 293, 355, 416, 476, 534, 589, 642)
  vol_remove <- c(16,  29,  35,  39,  39,  39,  38,  37,  36,  35,  34)

  age[3] <- NA

  expect_error(stand_level_increment(age, vol_remain, vol_remove))

  age        <- seq(20, 70, 5)
  vol_remain <- c(65, 118, 175, 233, 293,  NA, 416, 476, 534, 589, 642)
  vol_remove <- c(16,  29,  35,  39,  39,  39,  38,  37,  36,  35,  34)

  expect_error(stand_level_increment(age, vol_remain, vol_remove))

  age        <- seq(20, 70, 5)
  vol_remain <- c(65, 118, 175, 233, 293,  NA, 416, 476, 534, 589, 642)
  vol_remove <- c(16,  29,  35,  NA,  NA,  39,  38,  37,  36,  35,  NA)

  expect_error(stand_level_increment(age, vol_remain, vol_remove))
})


test_that("unordered time vectors trigger an error", {

  time <- c(4, 2, 3, 1)
  rmn  <- c(1, 2, 3, 4)
  rmv  <- c(1, 2, 3, 4)

  expect_error(stand_level_increment(time, rmn, rmv))
})


test_that("time vectors containing duplicates trigger an error", {

  time <- c(1, 2, 2, 4)
  rmn  <- c(1, 2, 3, 4)
  rmv  <- c(1, 2, 3, 4)

  expect_error(stand_level_increment(time, rmn, rmv))
})



