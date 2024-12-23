

library(ForestElementsR)


test_that("different options for outline and n_rep_ha are handled correctly", {
  area_ha <- 70 * 70 / 10000
  n_rep_ha <- 1 / area_ha
  n_rep_ha_with_na <- rep(n_rep_ha, 100)
  n_rep_ha_with_na[seq(2, 100, by = 2)] <- NA

  trees <- tibble::tibble(
    tree_no    = as.character(c(1:100)),
    species_id = as_fe_species_tum_wwk_short(c(rep("1", 30), rep("5", 70))),
    time_yr    = 2022,
    dbh        = c(rnorm(30, 45, 12), rnorm(70, 38, 9))
  )

  tree_positions <- tibble::tibble(
    tree_no = trees$tree_no,
    x = 70 * runif(100),
    y = 70 * runif(100)
  )

  outline <- tibble::tibble(
    x = c(0, 0, 70, 70),
    y = c(0, 70, 70, 0)
  )

  outline_bad <- tibble::tibble( # Malformed polygon
    x = c(0, 70, 70, 0),
    y = c(0, 70, 0, 70)
  )


  # Raw stand objects for different test cases
  # proper outline, no n_rep_ha given
  test_stand_raw_1 <- list(
    trees = trees, tree_positions = tree_positions,
    outline = outline
  )

  # no outline, but complete n_rep_ha
  test_stand_raw_2 <- list(trees = trees, tree_positions = tree_positions)
  test_stand_raw_2$trees <- test_stand_raw_2$trees |>
    dplyr::mutate(n_rep_ha = n_rep_ha)

  # proper outline, but incomplete n_rep_ha
  test_stand_raw_3 <- test_stand_raw_2
  test_stand_raw_3$outline <- outline
  test_stand_raw_3$trees <- test_stand_raw_3$trees |>
    dplyr::mutate(n_rep_ha = n_rep_ha_with_na)

  # proper outline, and complete n_rep_ha
  test_stand_raw_4 <- test_stand_raw_2
  test_stand_raw_4$outline <- outline
  test_stand_raw_4$trees <- test_stand_raw_4$trees |>
    dplyr::mutate(n_rep_ha = n_rep_ha)

  # no outline given, but incomplete n_rep_ha
  test_stand_raw_5 <- list(trees = trees, tree_positions = tree_positions)
  test_stand_raw_5$trees <- test_stand_raw_5$trees |>
    dplyr::mutate(n_rep_ha = n_rep_ha_with_na)

  # bad outline given
  test_stand_raw_6 <- test_stand_raw_1
  test_stand_raw_6$outline <- outline_bad


  # If outline is given, fe_stand_spatial should hand back an fe_stand_spatial
  # object
  expect_true(
    inherits(
      fe_stand_spatial(
        test_stand_raw_1,
        tree_id_col    = "tree_no",
        species_id_col = "species_id",
        time_yr_col    = "time_yr",
        dbh_cm_col     = "dbh",
        x_m_col        = "x",
        y_m_col        = "y"
      ), "fe_stand_spatial"
    )
  )

  # If outline is not given (i.e. outline_frame_name NULL),
  # but n_rep_ha is complete, fe_stand should hand back an fe_stand_spatial
  # object
  expect_true(
    inherits(
      fe_stand_spatial(
        test_stand_raw_2,
        outline_frame_name = NULL,
        tree_id_col = "tree_no",
        species_id_col = "species_id",
        time_yr_col = "time_yr",
        dbh_cm_col = "dbh",
        n_rep_ha_col = "n_rep_ha",
        x_m_col = "x",
        y_m_col = "y"
      ), "fe_stand_spatial"
    )
  )

  # If outline is given AND and there are values given for n_rep_ha, a warning
  # must be issued, no matter whether n_rep_ha has missing data ...
  expect_warning(
    fe_stand_spatial(
      test_stand_raw_3,
      tree_id_col    = "tree_no",
      species_id_col = "species_id",
      time_yr_col    = "time_yr",
      dbh_cm_col     = "dbh",
      n_rep_ha_col   = "n_rep_ha",
      x_m_col        = "x",
      y_m_col        = "y"
    )
  )

  # ... or no missing data
  expect_warning(
    fe_stand_spatial(
      test_stand_raw_4,
      tree_id_col    = "tree_no",
      species_id_col = "species_id",
      time_yr_col    = "time_yr",
      dbh_cm_col     = "dbh",
      n_rep_ha_col   = "n_rep_ha",
      x_m_col        = "x",
      y_m_col        = "y"
    )
  )


  # An error must be thrown, however, if no outline is given, and
  # there are missing values in n_rep_ha
  expect_error(
    fe_stand_spatial(
      test_stand_raw_5,
      outline_frame_name = NULL,
      tree_id_col = "tree_no",
      species_id_col = "species_id",
      time_yr_col = "time_yr",
      dbh_cm_col = "dbh",
      n_rep_ha_col = "n_rep_ha",
      x_m_col = "x",
      y_m_col = "y"
    )
  )


  # Malformed outline - must trigger an error
  expect_error(
    fe_stand_spatial(
      test_stand_raw_6,
      tree_id_col    = "tree_no",
      species_id_col = "species_id",
      time_yr_col    = "time_yr",
      dbh_cm_col     = "dbh",
      n_rep_ha_col   = "n_rep_ha",
      x_m_col        = "x",
      y_m_col        = "y"
    )
  )
})
