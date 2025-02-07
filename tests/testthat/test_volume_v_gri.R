
library(ForestElementsR)


test_that("volume calculation v_gri is correct", {

  # Three trees with different dimensions replicated for all ten species
  # (groups) for which v_gri is parameterized
  d_cm <- rep(c(15.3, 33.7, 52.1), times = 10)
  h_m  <- rep(c(16.0, 30.0, 42.5), times = 10)
  spec_tum_wwk_short <- fe_species_tum_wwk_short(rep(1:10, each = 3))
  spec_bvr_ste_short <- fe_species_bavrn_state_short(rep(1:9, each = 3))

  # values to compare with - made as: v_gri(spec_tum_wwk_short, d_cm, h_m)
  v_expect_tum_wwk_short <- c(
   0.1371064961008938987153, 1.2847250149801376917225, 4.0270109740259369957016,
   0.1485542424905365033183, 1.3481133160341336463262, 4.2517482044334844459854,
   0.1278176720975175151285, 1.2094627399064568074039, 4.0837076608513092068620,
   0.1323672000770205747777, 1.2024180784821636436277, 3.8305191099594839698739,
   0.1347264542610926429944, 1.3866614712839804290923, 4.8134274473515024084236,
   0.1352063205996395511832, 1.3903095953196904499549, 4.8259814917517731558405,
   0.1259655798445232788296, 1.1800351080494451139202, 3.8327873931486911018851,
   0.1347264542610926429944, 1.3866614712839804290923, 4.8134274473515024084236,
   0.1192459401632156440476, 1.3214073224258306993306, 4.7402175978377583120960,
   0.1372201447764449799838, 1.3210884954163617521772, 4.0966251983266550595886
  )

  # values to compare with - made as:
  # v_gri(spec_bvr_ste_short, d_cm[1:9], h_m[1:9]) # only 9 species (groups)
  # that coding
  v_expect_bavrn_state_short <- c(
   0.1371064961008938987153, 1.2847250149801376917225, 4.0270109740259369957016,
   0.1278176720975175151285, 1.2094627399064568074039, 4.0837076608513092068620,
   0.1485542424905365033183, 1.3481133160341336463262, 4.2517482044334844459854,
   0.1323672000770205747777, 1.2024180784821636436277, 3.8305191099594839698739,
   0.1259655798445232788296, 1.1800351080494451139202, 3.8327873931486911018851,
   0.1347264542610926429944, 1.3866614712839804290923, 4.8134274473515024084236,
   0.1352063205996395511832, 1.3903095953196904499549, 4.8259814917517731558405,
   0.1192459401632156440476, 1.3214073224258306993306, 4.7402175978377583120960,
   0.1347264542610926429944, 1.3866614712839804290923, 4.8134274473515024084236
 )

  # standard usage
  expect_equal(v_gri(spec_tum_wwk_short, d_cm, h_m), v_expect_tum_wwk_short)
  # usage with bavrn_state_short species coding (where there is also no
  expect_equal(
    v_gri(spec_bvr_ste_short, d_cm[1:(9*3)], h_m[1:(9*3)]),
    v_expect_bavrn_state_short
  )
  # trees with a dbh < 6.1 have volume == 0 by definition (merchantable wood)
  expect_equal(v_gri(1, 6.0, 7.0), 0)
})



test_that("volume calculation v_gri works for all species codings", {

  ## 1. Test species codings compatible with tum_wwk_short
  spec_raw <- fe_species_get_coding_table("tum_wwk_short")$species_id |>
    unique()
  d_cm <- rep(30, times = length(spec_raw))
  h_m  <- rep(29, times = length(spec_raw))

  # Coding as numeric (but tum_wwk_short codes) must yield the same results
  # as with tum_wwk_short
  spec_num           <- as.numeric(spec_raw)
  spec_tum_wwk_short <- as_fe_species_tum_wwk_short(spec_raw)

  expect_equal(v_gri(spec_num, d_cm, h_m), v_gri(spec_tum_wwk_short, d_cm, h_m))

  # Coding as character (but tum_wwk_short codes) must yield the same results
  # as with tum_wwk_short
  # Assign and "convert" below just for safety and clarity
  # (spec_raw *is* character)
  spec_char <- as.character(spec_raw)

  # Coding as character (but tum_wwk_short codes)
  expect_equal(
    v_gri(spec_char, d_cm, h_m), v_gri(spec_tum_wwk_short, d_cm, h_m)
  )


  # TUM WWK long coding
  spec_raw <- fe_species_get_coding_table("tum_wwk_long")$species_id |>
    unique()
  d_cm <- rep(30, times = length(spec_raw))
  h_m  <- rep(29, times = length(spec_raw))

  # Calculated volumes with both codings must be equal
  # species cast warnings are ok
  spec_tum_wwk_long  <- as_fe_species_tum_wwk_long(spec_raw)
  spec_tum_wwk_short <-
    suppressWarnings(as_fe_species_tum_wwk_short(spec_tum_wwk_long))

  expect_equal(
    suppressWarnings( # species cast warnings are ok
      v_gri(spec_tum_wwk_long, d_cm, h_m),
    ),
    v_gri(spec_tum_wwk_short, d_cm, h_m)
  )


  # German national forest inventory coding
  spec_raw <- fe_species_get_coding_table("ger_nfi_2012")$species_id |>
    unique()

  d_cm <- rep(30, times = length(spec_raw))
  h_m  <- rep(29, times = length(spec_raw))

  # Calculated volumes with both codings must be equal
  # species cast warnings are ok
  spec_ger_nfi_2012  <- as_fe_species_ger_nfi_2012(spec_raw)
  spec_tum_wwk_short <-
    suppressWarnings(as_fe_species_tum_wwk_short(spec_ger_nfi_2012))

  expect_equal(
    suppressWarnings( # species cast warnings are ok
      v_gri(spec_ger_nfi_2012, d_cm, h_m),
    ),
    v_gri(spec_tum_wwk_short, d_cm, h_m)
  )


  # ForestElementsR master coding
  spec_raw <- fe_species_get_coding_table("master")$species_id |>
    unique()

  d_cm <- rep(30, times = length(spec_raw))
  h_m  <- rep(29, times = length(spec_raw))

  # Calculated volumes with both codings must be equal
  # species cast warnings are ok
  spec_master        <- as_fe_species_master(spec_raw)
  spec_tum_wwk_short <-
    suppressWarnings(as_fe_species_tum_wwk_short(spec_master))

  expect_equal(
    suppressWarnings( # species cast warnings are ok
      v_gri(spec_master, d_cm, h_m)
    ),
    v_gri(spec_tum_wwk_short, d_cm, h_m)
  )


  ## 2. Test species codings compatible with bavrn_state_short coding
  spec_raw <- fe_species_get_coding_table("bavrn_state")$species_id |> unique()
  d_cm <- rep(30, times = length(spec_raw))
  h_m  <- rep(29, times = length(spec_raw))

  # Calculated volumes with both codings must be equal
  # species cast warnings are ok
  spec_bvrn_state       <- as_fe_species_bavrn_state(spec_raw)
  spec_bvrn_state_short <-
    suppressWarnings(as_fe_species_bavrn_state_short(spec_bvrn_state))

  expect_equal(
    suppressWarnings( # species cast warnings are ok
      v_gri(spec_bvrn_state, d_cm, h_m)
    ),
    v_gri(spec_bvrn_state_short, d_cm, h_m)
  )
})


test_that("volumes for tum_wwk_short and bavrn_state_short match where they should", {

  test_tib <- tibble::tibble(
    spec_tum = c(1, 2, 3, 4, 5, 6, 7, 8, 9) |>
      as_fe_species_tum_wwk_short(),
    spec_bav = c(1, 3, 2, 4, 6, 7, 5, 9, 8) |>
      as_fe_species_bavrn_state_short(),
    v_tum = v_gri(spec_tum, 25, 24),
    v_bav = v_gri(spec_bav, 25, 24)
  )

  expect_equal(test_tib$v_tum, test_tib$v_bav)
})


test_that("Missing value handling is correct", {

  expect_error(
    # Missing species_id
    v_gri(c(rep(1, 4), NA), c(30, 24, 50, 42, 17), c(29, 19, 39, 37, 18))
  )
  expect_error(
    # Missing dbh_cm
    v_gri(rep(1, 5), c(30, NA, 50, 42, 17), c(29, 19, 39, 37, 18))
  )
  expect_warning(
    # Missing height_m
    v_test <- v_gri(rep(1, 5), c(30, 24, 50, 42, 17), c(29, 19, 39, NA, 18))
  )
  expect_true(
    # Missing height_m generates NA result
    is.na(v_test[4])
  )
  expect_true(
    # Non-missing height_m generate non-NA result even though there are NAs in
    # the same input vector
    all(!is.na(v_test[c(1, 2, 3, 5)]))
  )
})





