
library(ForestElementsR)


# Casting invariants the package relies on (see also the "Casting-Invarianten"
# section in the developer notes): every species/coding that should reach
# tum_wwk_short must do so, and bavrn_state must reach bavrn_state_short. These
# are robust to the master table gaining species over time (unlike a test on a
# fixed row count) - a new species simply has to find a home in the goal coding.
# Information-loss messages from casting into groups are expected and silenced.


test_that("every master species converts to tum_wwk_short", {
  spec <- as_fe_species_master(
    fe_species_get_coding_table("master")$species_id |> unique()
  )
  expect_no_error(suppressMessages(as_fe_species_tum_wwk_short(spec)))
})



test_that("every tum_wwk_long code converts to tum_wwk_short", {
  spec <- as_fe_species_tum_wwk_long(
    fe_species_get_coding_table("tum_wwk_long")$species_id |> unique()
  )
  expect_no_error(suppressMessages(as_fe_species_tum_wwk_short(spec)))
})



test_that("every bavrn_state code converts to bavrn_state_short", {
  spec <- as_fe_species_bavrn_state(
    fe_species_get_coding_table("bavrn_state")$species_id |> unique()
  )
  expect_no_error(suppressMessages(as_fe_species_bavrn_state_short(spec)))
})
