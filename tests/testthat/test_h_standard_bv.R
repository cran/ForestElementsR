
library(ForestElementsR)



test_that("badly dimensioned inputs trigger an error", {

  # No input vector must be longer than dbh_cm
  expect_error(
    h_standard_bv(species_id = c(1, 1, 1),
                  dbh_cm = c(32, 33),
                  age_yr = c(50, 50),
                  d_q_cm = 29,
                  h_q_m  = 28)
  )

  expect_error(
    h_standard_bv(species_id = c(1, 1, 1),
                  dbh_cm = c(32, 33),
                  age_yr = c(50, 50, 50),
                  d_q_cm = 29,
                  h_q_m  = 28)
  )

  expect_error(
    h_standard_bv(species_id = 1,
                  dbh_cm = c(32, 33),
                  age_yr = c(50, 50),
                  d_q_cm = c(29, 29.3, 31.2),
                  h_q_m  = 28)
  )

  expect_error(
    h_standard_bv(species_id = 1,
                  dbh_cm = c(32, 33),
                  age_yr = c(50, 50),
                  d_q_cm = c(29.3),
                  h_q_m  = c(28, 28.8, 28.8))
  )

  # Having only dbh_cm with length > 1 is ok
  expect_no_error(
    h_standard_bv(species_id = 1,
                  dbh_cm = c(32, 33, 35, 52.3),
                  age_yr = 72,
                  d_q_cm = 29.3,
                  h_q_m  = 28)
  )
})




test_that("height calculation h_standard_bv is correct", {

  # tum_wwk_short coding
  # Three trees with different diameters replicated for all ten species
  # (groups) for which h_standard_bv is parameterized
  d_cm <- rep(c(15.3, 33.7, 52.1), times = 10)
  spec <- fe_species_tum_wwk_short(rep(1:10, each = 3))
  age  <- 79
  dq   <- 35.2
  hq   <- 34.8

  h_expect <- c(
    22.58594055174814840825, 34.24495713421811160515, 39.24720086319105405437,
    23.89606258100883096063, 34.32220154640431530879, 38.57658526165201351432,
    26.99023641611186263845, 34.48383618006626250008, 37.22994938541503273655,
    27.74993398845824188470, 34.51972948664928964035, 36.94088777804995515908,
    26.07623706293851384430, 34.43880315067882946778, 37.59765614121992882701,
    26.10578171604564090558, 34.44029199232295468391, 37.58540863224157391187,
    29.29907198027138193197, 34.58898005576661915939, 36.39303566576278115008,
    27.47525170695753260475, 34.50690614225712948837, 37.04375377472279495805,
    27.47525170695753260475, 34.50690614225712948837, 37.04375377472279495805,
    27.00761570490848129111, 34.48467246548143094742, 37.22317421429097095142
  )

  expect_equal(h_standard_bv(spec, d_cm, age, dq, hq), h_expect)


  # bavrn_state_short coding
  # Three trees with different diameters replicated for all nine species
  # (groups) for which h_standard_bv is parameterized
  d_cm <- rep(c(15.3, 33.7, 52.1), times = 9)
  spec <- fe_species_bavrn_state_short(rep(1:9, each = 3))
  age  <- 79
  dq   <- 35.2
  hq   <- 34.8

  h_expect <- c(
    22.58594055174814840825, 34.24495713421811160515, 39.24720086319105405437,
    26.99023641611186263845, 34.48383618006626250008, 37.22994938541503273655,
    23.89606258100883096063, 34.32220154640431530879, 38.57658526165201351432,
    27.74993398845824188470, 34.51972948664928964035, 36.94088777804995515908,
    29.29907198027138193197, 34.58898005576661915939, 36.39303566576278115008,
    26.07623706293851384430, 34.43880315067882946778, 37.59765614121992882701,
    26.10578171604564090558, 34.44029199232295468391, 37.58540863224157391187,
    27.47525170695753260475, 34.50690614225712948837, 37.04375377472279495805,
    27.47525170695753260475, 34.50690614225712948837, 37.04375377472279495805
  )

  expect_equal(h_standard_bv(spec, d_cm, age, dq, hq), h_expect)


  # tum_wwk_long coding: three trees per (tree) species. The function casts
  # tum_wwk_long -> tum_wwk_short (a valid coarsening), so each species must give
  # exactly its short group's value. Tested as that invariant (order- and
  # edit-robust); absolute values are pinned in the tum_wwk_short block above.
  ct   <- fe_species_get_coding_table("tum_wwk_long")
  ids  <- unique(ct$species_id[ct$is_tree])
  spec <- rep(as_fe_species_tum_wwk_long(ids), each = 3)
  d_cm <- rep(c(15.3, 33.7, 52.1), times = length(ids))
  age  <- 79
  dq   <- 35.2
  hq   <- 34.8

  expect_equal(
    suppressWarnings(h_standard_bv(spec, d_cm, age, dq, hq)),
    suppressWarnings(h_standard_bv(
      as_fe_species_tum_wwk_short(spec), d_cm, age, dq, hq
    ))
  )


  # bavrn_state coding: three trees per (tree) species. The function casts
  # bavrn_state -> bavrn_state_short, so each species must give exactly its
  # short group's value. Tested as that invariant (order- and edit-robust);
  # absolute values are pinned in the tum_wwk_short / bavrn_state_short blocks.
  ct   <- fe_species_get_coding_table("bavrn_state")
  ids  <- sort(unique(ct$species_id[ct$is_tree]))  # exclude non-tree (e.g. 99)
  spec <- rep(as_fe_species_bavrn_state(ids), each = 3)
  d_cm <- rep(c(15.3, 33.7, 52.1), times = length(ids))
  age  <- 79
  dq   <- 35.2
  hq   <- 34.8

  expect_equal(
    suppressWarnings(h_standard_bv(spec, d_cm, age, dq, hq)),
    suppressWarnings(h_standard_bv(
      as_fe_species_bavrn_state_short(spec), d_cm, age, dq, hq
    ))
  )
})




test_that("height calculation h_standard_bv works with all species codings", {

  d_cm  <- rep(30, 5)
  spec_raw <- c(1, 2, 3, 5, 7)
  age   <- 63
  dq    <- 27.2
  hq    <- 27.1
  spec  <- as_fe_species_tum_wwk_short(spec_raw)

  # Reference calculated with tum_wwk_short
  h_ref <- h_standard_bv(spec, d_cm, age, dq, hq)

  # Coding as numeric (but tum_wwk_short codes)
  expect_equal(
    h_standard_bv(spec_raw, d_cm, age, dq, hq),
    h_ref
  )
  # Coding as character (but tum_wwk_short codes)
  expect_equal(
    h_standard_bv(as.character(spec_raw), d_cm, age, dq, hq),
    h_ref
  )
  # Bavarian state coding
  expect_equal(
    h_standard_bv(as_fe_species_bavrn_state(spec), d_cm, age, dq, hq),
    h_ref
  )
  # German national forest inventory coding
  expect_equal(
    h_standard_bv(as_fe_species_ger_nfi_2012(spec), d_cm, age, dq, hq),
    h_ref
  )
  # TUM WWK long coding
  expect_equal(
    h_standard_bv(as_fe_species_tum_wwk_long(spec), d_cm, age, dq, hq),
    h_ref
  )
  # ForestElementsR master coding
  expect_equal(
    h_standard_bv(as_fe_species_master(spec), d_cm, age, dq, hq),
    h_ref
  )
})



