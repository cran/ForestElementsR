
library(ForestElementsR)

# test the function h_q_from_d_q


test_that("height calculation h_q_from_d_q is correct", {

  # Three trees with different diameters replicated for all ten species
  # (groups) for which v_gri is parameterized
  d_q_cm <- rep(c(15.3, 33.7, 52.1), times = 10)
  spec   <- fe_species_tum_wwk_short(rep(1:10, each = 3))

  h_q_m_expect <- c(
    25.70202558467983067203, 37.78385426726290319266, 42.30711966988408079260,
    25.70202558467983067203, 37.78385426726290319266, 42.30711966988408079260,
    16.08597779340455247166, 24.76217085508127979665, 28.69060586661925427165,
    25.70202558467983067203, 37.78385426726290319266, 42.30711966988408079260,
    18.49098890076501433555, 30.17763333918726331717, 36.41166248268504546104,
    18.49098890076501433555, 30.17763333918726331717, 36.41166248268504546104,
    25.70202558467983067203, 37.78385426726290319266, 42.30711966988408079260,
    18.49098890076501433555, 30.17763333918726331717, 36.41166248268504546104,
    18.49098890076501433555, 30.17763333918726331717, 36.41166248268504546104,
    25.70202558467983067203, 37.78385426726290319266, 42.30711966988408079260
  )

  expect_equal(h_q_from_d_q(spec, d_q_cm), h_q_m_expect)
})



test_that("height calculation h_q_from_d_q works with all species codings", {

  d_q_cm   <- rep(30, 5)
  spec_raw <- c(1, 2, 3, 5, 7)
  spec     <- as_fe_species_tum_wwk_short(spec_raw)

  # Reference calculated with tum_wwk_short
  h_ref <- h_q_from_d_q(spec, d_q_cm)

  # Coding as numeric (but tum_wwk_short codes)
  expect_equal(
    h_q_from_d_q(spec_raw, d_q_cm),
    h_ref
  )
  # Coding as character (but tum_wwk_short codes)
  expect_equal(
    h_q_from_d_q(as.character(spec_raw), d_q_cm),
    h_ref
  )
  # Bavarian state coding
  expect_equal(
    h_q_from_d_q(as_fe_species_bavrn_state(spec), d_q_cm),
    h_ref
  )
  # German national forest inventory coding
  expect_equal(
    h_q_from_d_q(as_fe_species_ger_nfi_2012(spec), d_q_cm),
    h_ref
  )
  # TUM WWK long coding
  expect_equal(
    h_q_from_d_q(as_fe_species_tum_wwk_long(spec), d_q_cm),
    h_ref
  )
  # ForestElementsR master coding
  expect_equal(
    h_q_from_d_q(as_fe_species_master(spec), d_q_cm),
    h_ref
  )
})



test_that("badly dimensioned inputs trigger an error", {

  # If the input vectors have different lengths, one of them must not be longer
  # than 1
  expect_error(
    h_q_from_d_q(species_id = c(1, 1, 1), d_q_cm = c(32, 33))
  )
  expect_error(
    h_q_from_d_q(species_id = c(1, 1), d_q_cm = c(32, 33, 48))
  )

  # Unequal lengths of the input vectors are acceptable, if the shorter is not
  # longer than 1
  expect_no_error(
    h_q_from_d_q(species_id = 1, d_q_cm = c(32, 33, 48))
  )
  expect_no_error(
    h_q_from_d_q(species_id = c(1, 3, 7), d_q_cm = 32)
  )
})


