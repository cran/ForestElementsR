
library(ForestElementsR)


test_that("standing_area_gnfi3 works with all appropriate species codings", {

  # Original parameterization
  spec_id <- as_fe_species_ger_nfi_2012(
    fe_species_get_coding_table("ger_nfi_2012")$species_id |>
      unique()
  )

  dbh_cm <- rep(30, times = 51)

  areas_exp <- round(
    c(
      16.63373776762521671913, 16.63373776762521671913, 16.63373776762521671913,
      22.20575041173110264481, 22.20575041173110264481, 22.20575041173110264481,
      22.20575041173110264481, 22.20575041173110264481, 22.20575041173110264481,
      16.98716694115406866672, 16.98716694115406866672, 16.98716694115406866672,
      19.13716694115407079835, 25.14546289114454680202, 23.37831702350028706405,
      16.63373776762521671913, 16.63373776762521671913, 22.53575041173110093951,
      29.03090470877928197524, 29.03090470877928197524, 27.24004214701962212075,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      39.61006322052943318113, 39.61006322052943318113, 33.24833809701009812443,
      33.24833809701009812443, 45.61946710584651043519, 45.61946710584651043519,
      45.61946710584651043519, 45.61946710584651043519, 45.61946710584651043519,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037,
      25.82632545290421433037, 25.82632545290421433037, 25.82632545290421433037
    ),
    digits = 5
  )

  expect_equal(
    round(standing_area_gnfi3(spec_id, dbh_cm), digits = 5), areas_exp
  )

  # -----
  # tum_wwk_long: casts tum_wwk_long -> tum_wwk_short (a valid coarsening), so
  # every species must yield exactly its short group's value. Tested as that
  # invariant (order- and edit-robust); absolute values are pinned in the
  # tum_wwk_short block below.
  ct  <- fe_species_get_coding_table("tum_wwk_long")
  ids <- unique(ct$species_id[ct$is_tree])
  sp_long  <- as_fe_species_tum_wwk_long(ids)
  sp_short <- suppressWarnings(as_fe_species_tum_wwk_short(sp_long))
  dbh_cm   <- rep(30, times = length(ids))

  expect_equal(
    suppressWarnings(standing_area_gnfi3(sp_long, dbh_cm)),
    standing_area_gnfi3(sp_short, dbh_cm)
  )


  # -----
  # tum_wwk_short
  spec_id <- as_fe_species_tum_wwk_short(
    fe_species_get_coding_table("tum_wwk_short")$species_id |>
      unique()
  )

  dbh_cm <- rep(30, times = 10)

  areas_exp <- round(
    c(
      16.63373776762521671913, 16.98716694115406866672, 22.20575041173110264481,
      25.14546289114454680202, 22.53575041173110093951, 29.03090470877928197524,
      19.13716694115407079835, 25.82632545290421433037, 33.24833809701009812443,
      16.63373776762521671913
    ),
    digits = 5
  )

  expect_equal(
    round(
      suppressWarnings( # Species cast warning is not a problem
        standing_area_gnfi3(spec_id, dbh_cm)
      ),
      digits = 5),
    areas_exp
  )

  # -----
  # bavrn_state: the function casts bavrn_state -> bavrn_state_short, so every
  # (tree) species must yield exactly its short group's value. Tested as that
  # invariant (order-insensitive, robust to coding edits) rather than a pinned
  # vector; the absolute values are pinned in the bavrn_state_short block below.
  ct  <- fe_species_get_coding_table("bavrn_state")
  ids <- unique(ct$species_id[ct$is_tree]) # exclude non-tree codes (e.g. 99)
  sp_bs    <- as_fe_species_bavrn_state(ids)
  sp_short <- suppressMessages(as_fe_species_bavrn_state_short(sp_bs))
  dbh_cm   <- rep(30, times = length(ids))

  expect_equal(
    suppressWarnings(standing_area_gnfi3(sp_bs, dbh_cm)),
    standing_area_gnfi3(sp_short, dbh_cm)
  )

  # -----
  # bavrn_state_short (explicit codes 1..9, independent of coding-table order)
  spec_id <- as_fe_species_bavrn_state_short(as.character(1:9))

  dbh_cm <- rep(30, times = 9)

  areas_exp <- c(
    16.63374, 22.20575, 16.98717, 25.14546, 19.13717, 22.53575,
    29.0309, 33.24834, 25.82633
  )

  expect_equal(
    round(
      suppressWarnings( # Species cast warning is not a problem
        standing_area_gnfi3(spec_id, dbh_cm)
      ),
      digits = 5),
    areas_exp
  )

  # -----
  # Master coding (exercises the default standing_area_gnfi3() method). A fixed
  # representative set keeps this independent of the master table's size; that
  # *every* master species converts to tum_wwk_short is checked separately in
  # test_coding_convertibility.R.
  spec_id <- as_fe_species_master(c(
    "picea_001", "abies_001", "pinus_001", "pseudotsuga_001", "larix_001",
    "fagus_001", "quercus_001", "acer_001", "alnus_002", "betula_001"
  ))

  dbh_cm <- rep(30, times = 10)

  areas_exp <- c(
    16.63374, 16.98717, 22.20575, 19.13717, 25.14546, 22.53575,
    29.0309, 25.82633, 33.24834, 39.61006
  )

  expect_equal(
    round(
      suppressWarnings( # Species cast warning is not a problem
        standing_area_gnfi3(spec_id, dbh_cm)
      ),
      digits = 5),
    areas_exp
  )
})


