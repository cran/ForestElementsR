
library(ForestElementsR)



test_that("quadratic mean diameter calculation is correct", {
  d_cm <- c(12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42)
  n_rep_a <- c(3.33, 1.27, 1.27, 1.27, 1.27, 5, 5, 5, 5, 3.42, 4.2)
  n_rep_b <- c(3.33, 0, 1.27, 0, 1.27, 0, 5, 0, 5, 0, 4.2)

  # default n_rep
  expect_equal(d_q(d_cm), 28.5302072770726162787)
  # recycled constant n_rep
  expect_equal(d_q(d_cm, 2.667), 28.5302072770726162787)
  # fully given n_rep
  expect_equal(d_q(d_cm, n_rep_a), 30.18307227222197397509)
  # fully given n_rep but partly zero
  expect_equal(d_q(d_cm, n_rep_b), 31.31025387963143913339)
  # zero n_rep (must result in NaN)
  expect_true(is.nan(d_q(d_cm, 0)))
  # n_rep length not matching
  expect_error(d_q(d_cm, c(3, 2)))
})



test_that("quadratic mean height calculation is correct", {
  d_cm <- c(12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42)
  h_m <- c(13.8, 14, 23.7, 25.3, 26.0, 24.5, 25.1, 29.0, 31.3, 28.4, 35.8)
  n_rep_a <- c(3.33, 1.27, 1.27, 1.27, 1.27, 5, 5, 5, 5, 3.42, 4.2)
  n_rep_b <- c(3.33, 0, 1.27, 0, 1.27, 0, 5, 0, 5, 0, 4.2)

  # default n_rep
  expect_equal(h_q(h_m, d_cm), 28.43776159576487927438)
  # recycled constant n_rep
  expect_equal(h_q(h_m, d_cm, 2.667), 28.43776159576487927438)
  # fully given n_rep
  expect_equal(h_q(h_m, d_cm, n_rep_a), 29.34370094385769789369)
  # fully given n_rep but partly zero
  expect_equal(h_q(h_m, d_cm, n_rep_b), 30.79555431055173642108)
  # zero n_rep (must result in NaN)
  expect_true(is.nan(h_q(h_m, d_cm, 0)))
  # n_rep length not matching
  expect_error(h_q(h_m, d_cm, c(3, 2)))
})



test_that("dominant diameter (d100) calculation is correct", {
  d_cm <- c(
    12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42
  )
  # rounded rep-numbers according to angle count sampling with factor 4 m²/ha
  n_rep_a <- c(
    315.8, 301.4, 80.8, 68.8, 64.0, 74.8, 70.4, 48.8, 40.4, 51.7, 28.9
  )
  # rep numbers reduced to sum < 100
  n_rep_b <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 40.4, 0, 28.9
  )

  # standard usage
  expect_equal(d_100(d_cm, n_rep_a), 36.59545094680484567107)
  # under usual circumstances d_100 > d_q ...
  expect_true(d_100(d_cm, n_rep_a) > d_q(d_cm, n_rep_a))
  # but if sum(n_rep) <= 100 d_100 == d_q
  expect_equal(d_100(d_cm, n_rep_b), d_q(d_cm, n_rep_b))
  # n_rep length not matching
  expect_error(d_100(d_cm, c(3, 2)))
})



test_that("dominant diameter (Weise) calculation is correct", {
  d_cm <- c(
    12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42
  )
  # rounded rep-numbers according to angle count sampling with factor 4 m²/ha
  n_rep_a <- c(
    315.8, 301.4, 80.8, 68.8, 64.0, 74.8, 70.4, 48.8, 40.4, 51.7, 28.9
  )
  # rep numbers reduced to sum < 100
  n_rep_b <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 40.4, 0, 28.9
  )

  # standard usage
  expect_equal(d_dom_weise(d_cm, n_rep_a), 33.09445207128245414196)

  # for 500 trees per ha d_dom_weise and d_100 must be the same
  expect_equal(
    d_dom_weise(d_cm, n_rep_a * 500 / sum(n_rep_a)),
    d_100(d_cm, n_rep_a * 500 / sum(n_rep_a))
  )

  # for less than 500 trees per ha d_dom_weise must be greater than d_100
  expect_true(d_dom_weise(d_cm, n_rep_a * 200 / sum(n_rep_a)) >
    d_100(d_cm, n_rep_a * 200 / sum(n_rep_a)))

  # for more than 500 trees per ha d_dom_weise must be smaller than d_100
  expect_true(d_dom_weise(d_cm, n_rep_a * 900 / sum(n_rep_a)) <
    d_100(d_cm, n_rep_a * 900 / sum(n_rep_a)))

  # d_dom_weise must be always greater than d_q
  expect_true(d_dom_weise(d_cm, n_rep_a) > d_q(d_cm, n_rep_a))

  # must work with partly zero representation numbers
  expect_equal(d_dom_weise(d_cm, n_rep_b), 42)

  # n_rep length not matching
  expect_error(d_dom_weise(d_cm, c(3, 2)))
})



test_that("dominant height (h100) calculation is correct", {
  d_cm <- c(
    12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42
  )
  h_m <- c(
    13.8, 14, 23.7, 25.3, 26.0, 24.5, 25.1, 29.0, 31.3, 28.4, 35.8
  )
  # rounded rep-numbers according to angle count sampling with factor 4 m²/ha
  n_rep_a <- c(
    315.8, 301.4, 80.8, 68.8, 64.0, 74.8, 70.4, 48.8, 40.4, 51.7, 28.9
  )
  # rep numbers reduced to sum < 100
  n_rep_b <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 40.4, 0, 28.9
  )

  # standard usage
  expect_equal(h_100(h_m, d_cm, n_rep_a), 32.46292077154386390703)
  # I do not test for h100 > hq for normal circumstances, because one could
  # think of h ~ d relations where the outcome is different, but ...
  # ... always must be true: if sum(n_rep) <= 100 h_100 == h_q
  expect_equal(h_100(h_m, d_cm, n_rep_b), h_q(h_m, d_cm, n_rep_b))
  # n_rep length not matching
  expect_error(h_100(h_m, d_cm, c(3, 2)))
})



test_that("dominant height (Weise) calculation is correct", {
  d_cm <- c(
    12.7, 13, 25.1, 27.2, 28.2, 26.1, 26.9, 32.3, 35.5, 31.4, 42
  )
  h_m <- c(
    13.8, 14, 23.7, 25.3, 26.0, 24.5, 25.1, 29.0, 31.3, 28.4, 35.8
  )
  # rounded rep-numbers according to angle count sampling with factor 4 m²/ha
  n_rep_a <- c(
    315.8, 301.4, 80.8, 68.8, 64.0, 74.8, 70.4, 48.8, 40.4, 51.7, 28.9
  )
  # rep numbers reduced to sum < 100
  n_rep_b <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 40.4, 0, 28.9
  )

  # standard usage
  expect_equal(h_dom_weise(h_m, d_cm, n_rep_a), 30.16167072513747982043)

  # for 500 trees per ha h_dom_weise and h_100 must be the same
  expect_equal(
    h_dom_weise(h_m, d_cm, n_rep_a * 500 / sum(n_rep_a)),
    h_100(h_m, d_cm, n_rep_a * 500 / sum(n_rep_a))
  )

  # for less than 500 trees per ha h_dom_weise must be greater than h_100
  expect_true(h_dom_weise(h_m, d_cm, n_rep_a * 200 / sum(n_rep_a)) >
    h_100(h_m, d_cm, n_rep_a * 200 / sum(n_rep_a)))

  # for more than 500 trees per ha h_dom_weise must be smaller than h_100
  expect_true(h_dom_weise(h_m, d_cm, n_rep_a * 900 / sum(n_rep_a)) <
    h_100(h_m, d_cm, n_rep_a * 900 / sum(n_rep_a)))

  # h_dom_weise must be always greater than h_q
  expect_true(h_dom_weise(h_m, d_cm, n_rep_a) > h_q(h_m, d_cm, n_rep_a))

  # must work with partly zero representation numbers
  expect_equal(h_dom_weise(h_m, d_cm, n_rep_b), 35.79999999999999715783)

  # n_rep length not matching
  expect_error(h_dom_weise(h_m, d_cm, c(3, 2)))
})
