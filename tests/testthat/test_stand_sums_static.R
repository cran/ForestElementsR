
library(ForestElementsR)



test_that("stand_sums_static works with missing height values", {

  # Reference for full information
  fe_stand_full <- spruce_beech_1_fe_stand
  # Remove every second height value in a copy
  index <- seq(1, nrow(fe_stand_full$trees), by = 2)
  fe_stand_red  <- fe_stand_full
  fe_stand_red$trees[index, ]$height_m <- NA

  # Must always work with full height information
  expect_no_error(
    rslt_full <- stand_sums_static(fe_stand_full)
  )
  # Must not throw an error with incomplete height information
  expect_no_error(
    rslt_red  <- stand_sums_static(fe_stand_red)
  )
  # The output parts that do not require heights must be identical for both
  # versions (complete and incomplete heights)
  expect_equal(
    rslt_full |> dplyr::select(
      time_yr, species_id, stem_number_ha, basal_area_m2_ha, d_q_cm, d_dom_cm
    ),
    rslt_red  |> dplyr::select(
      time_yr, species_id, stem_number_ha, basal_area_m2_ha, d_q_cm, d_dom_cm
    )
  )
  # The output variables that require heights must be NA for the reduced object
  expect_true(
    all(is.na(c(rslt_red$h_q_m, rslt_red$h_dom_m, rslt_red$v_m3_ha)))
  )
})
