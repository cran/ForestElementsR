
library(ForestElementsR)


test_that("crown_diameter_silva calculates correctly", {

  # Three trees with different dimensions replicated for all ten species
  # (groups) for which crown_diameter_Silva is parameterized
  d_cm <- rep(c(15.3, 33.7, 52.1), times = 10)
  h_m  <- rep(c(16.0, 30.0, 42.5), times = 10)
  spec <- fe_species_tum_wwk_short(rep(1:10, each = 3))

  # values to compare with - made as: crown_diameter_Silva(spec, d_cm, h_m)
  cd_expect <- c(
    2.793826022238466322278, 4.317200034675790831784, 5.723699093823009675930,
    3.421325252165424490869, 5.681024738596482492881, 7.503651222225220074336,
    3.017627426468806550730, 4.751258229967682211736, 5.922338450811990284706,
    2.605372241756146234337, 4.193659266869274482303, 5.803279630384463594339,
    5.411085353254330598816, 7.626692128455063723891, 9.061427214595209278514,
    3.019350107778851377560, 5.727765720328252818661, 9.326321688291686839989,
    3.517886371353495800207, 5.543301412683582007901, 7.190940842571755808876,
    4.306946895135291164536, 6.870122009783212746470, 9.523941330296244345277,
    3.153839198906947416390, 5.226169104287907707374, 6.901370308764652961031,
    2.793826022238466322278, 4.317200034675790831784, 5.723699093823009675930
  )

  # standard usage
  expect_equal(crown_diameter_silva(spec, d_cm, h_m), cd_expect)
})


test_that("crown_diameter_silva works for all species codings", {
  d_cm     <- rep(30, 5)
  h_m      <- rep(29, 5)
  spec_raw <- c(1, 2, 3, 5, 7)
  spec     <- as_fe_species_tum_wwk_short(spec_raw)
  # Calculate reference calculated with tum_wwk_short
  cd_ref <- crown_diameter_silva(spec, d_cm, h_m)

  # Coding as numeric (but tum_wwk_short codes)
  expect_equal(
    crown_diameter_silva(spec_raw, d_cm, h_m),
    cd_ref
  )
  # Coding as character (but tum_wwk_short codes)
  expect_equal(
    crown_diameter_silva(as.character(spec_raw), d_cm, h_m),
    cd_ref
  )
  # Bavarian state coding
  expect_equal(
    crown_diameter_silva(as_fe_species_bavrn_state(spec), d_cm, h_m),
    cd_ref
  )
  # German national forest inventory coding
  expect_equal(
    crown_diameter_silva(as_fe_species_ger_nfi_2012(spec), d_cm, h_m),
    cd_ref
  )
  # TUM WWK long coding
  expect_equal(
    crown_diameter_silva(as_fe_species_tum_wwk_long(spec), d_cm, h_m),
    cd_ref
  )
  # ForestElementsR master coding
  expect_equal(
    crown_diameter_silva(as_fe_species_master(spec), d_cm, h_m),
    cd_ref
  )
})
