
library(ForestElementsR)


test_that("different options for area_ha and n_rep_ha are handled correctly", {
  area_ha <- 0.33
  n_rep_ha <- 1 / area_ha
  n_rep_ha_with_na <- rep(n_rep_ha, 100)
  n_rep_ha_with_na[seq(2, 100, by = 2)] <- NA

  test_stand_raw <- tibble::tibble(
    tree_no    = as.character(c(1:100)),
    species_id = as_fe_species_tum_wwk_short(c(rep("1", 30), rep("5", 70))),
    time_yr    = 2022,
    dbh        = c(rnorm(30, 45, 12), rnorm(70, 38, 9))
  )


  # If area_ha is given, fe_stand should hand back an fe_stand object
  expect_true(
    inherits(
      fe_stand(
        test_stand_raw,
        tree_id_col    = "tree_no",
        species_id_col = "species_id",
        time_yr_col    = "time_yr",
        dbh_cm_col     = "dbh",
        area_ha        = area_ha
      ), "fe_stand"
    )
  )

  # If area_ha is not given (i.e. NULL), but n_rep_ha is complete, fe_stand
  # should hand back an fe_stand object
  expect_true(
    inherits(
      fe_stand(
        test_stand_raw |> dplyr::mutate(n_rep_ha = n_rep_ha),
        tree_id_col = "tree_no",
        species_id_col = "species_id",
        time_yr_col = "time_yr",
        dbh_cm_col = "dbh",
        n_rep_ha_col = "n_rep_ha",
        area_ha = NULL
      ), "fe_stand"
    )
  )


  # If area_ha is given AND and there are values given for n_rep_ha, a warning
  # must be issued, no matter whether n_rep_ha has missing data ...
  expect_warning(
    fe_stand(
      test_stand_raw |> dplyr::mutate(n_rep_ha = n_rep_ha_with_na),
      tree_id_col = "tree_no",
      species_id_col = "species_id",
      time_yr_col = "time_yr",
      dbh_cm_col = "dbh",
      n_rep_ha_col = "n_rep_ha",
      area_ha = area_ha
    )
  )

  # ... or no missing data
  expect_warning(
    fe_stand(
      test_stand_raw |> dplyr::mutate(n_rep_ha = n_rep_ha),
      tree_id_col = "tree_no",
      species_id_col = "species_id",
      time_yr_col = "time_yr",
      dbh_cm_col = "dbh",
      n_rep_ha_col = "n_rep_ha",
      area_ha = area_ha
    )
  )


  # An error must be thrown, however, if no value is given for area_ha, and
  # there are missing values in n_rep_ha
  expect_error(
    fe_stand(
      test_stand_raw |> dplyr::mutate(n_rep_ha = n_rep_ha_with_na),
      tree_id_col = "tree_no",
      species_id_col = "species_id",
      time_yr_col = "time_yr",
      dbh_cm_col = "dbh",
      n_rep_ha_col = "n_rep_ha",
      area_ha = NULL
    )
  )
})
