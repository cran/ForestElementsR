

library(ForestElementsR)


test_that("height_crown_base_silva calculates correctly", {

  # Three trees with different dimensions replicated for all ten species
  # (groups) for which height_crown_base_silva is parameterized
  d_cm <- rep(c(15.3, 33.7, 52.1), times = 10)
  h_m  <- rep(c(16.0, 30.0, 42.5), times = 10)
  spec <- fe_species_tum_wwk_short(rep(1:10, each = 3))

  # values to compare with - made as: height_crown_base_silva(spec, d_cm, h_m)
  hcb_expect <- c(
    9.954106949762170586382, 17.093389170119039022211, 23.119515367468430611098,
    8.887844412671467964060, 15.927221258786403268459, 22.858859184449418222584,
   10.112415760193904645803, 21.372436801349284252183, 33.689010607474486391766,
    9.245239276730764288459, 15.969712813884864743841, 21.625416431149638185616,
    8.033699814508343806096, 15.435292925762558624569, 22.558380048760650282702,
   10.988620228955744195787, 19.705536531820666112935, 26.784734451618994910405,
    8.748565618853309189262, 16.166644392981059041858, 24.192218018089452158392,
    7.417327099746957586035, 13.459018982013780885154, 18.756532459949387003917,
    9.789073525192815594664, 18.970643715613721980162, 28.045539832912972144641,
    9.954106949762170586382, 17.093389170119039022211, 23.119515367468430611098
  )

  # standard usage
  expect_equal(height_crown_base_silva(spec, d_cm, h_m), hcb_expect)
})


test_that("height_crown_base_silva works for all species codings", {
  d_cm     <- rep(30, 5)
  h_m      <- rep(29, 5)
  spec_raw <- c(1, 2, 3, 5, 7)
  spec     <- as_fe_species_tum_wwk_short(spec_raw)
  # Calculate reference calculated with tum_wwk_short
  hcb_ref <- height_crown_base_silva(spec, d_cm, h_m)

  # Coding as numeric (but tum_wwk_short codes)
  expect_equal(
    height_crown_base_silva(spec_raw, d_cm, h_m),
    hcb_ref
  )
  # Coding as character (but tum_wwk_short codes)
  expect_equal(
    height_crown_base_silva(as.character(spec_raw), d_cm, h_m),
    hcb_ref
  )
  # Bavarian state coding
  expect_equal(
    height_crown_base_silva(as_fe_species_bavrn_state(spec), d_cm, h_m),
    hcb_ref
  )
  # German national forest inventory coding
  expect_equal(
    height_crown_base_silva(as_fe_species_ger_nfi_2012(spec), d_cm, h_m),
    hcb_ref
  )
  # TUM WWK long coding
  expect_equal(
    height_crown_base_silva(as_fe_species_tum_wwk_long(spec), d_cm, h_m),
    hcb_ref
  )
  # ForestElementsR master coding
  expect_equal(
    height_crown_base_silva(as_fe_species_master(spec), d_cm, h_m),
    hcb_ref
  )
})


