
library(ForestElementsR)


test_that("forward ambiguous casts terminate with an error", {
  # Six ambiguous casts. Independent of the specific fe_species class, always
  # the same set of functions is used for casting, so we are triple safe here.
  # This test may require revision in case one of the codings used here gets
  # revised.

  # Goal "master" always covers every species, so casting a coarse group code
  # into it stays forward-ambiguous (one code -> many master codes) regardless
  # of which species the finer codings happen to cover.
  expect_error(
    as_fe_species_master(fe_species_tum_wwk_short("9")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_ger_nfi_2012(fe_species_bavrn_state("70")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_master(fe_species_tum_wwk_short("8")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_ger_nfi_2012("150")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_master(fe_species_ger_nfi_2012("90")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_ger_nfi_2012(fe_species_bavrn_state_short("4")),
    regexp = "^Ambiguous cast attempt\\."
  )

  # Casting a coarse group code into the now-complete, finer tum_wwk_long is
  # forward-ambiguous (one group -> many tum_wwk_long codes).
  expect_error(
    as_fe_species_tum_wwk_long(fe_species_bavrn_state_short("9")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_tum_wwk_short("9")),
    regexp = "^Ambiguous cast attempt\\."
  )
})



test_that("backward ambiguous casts emit a message", {
  # Six backward ambiguous casts. Independent of the specific fe_species
  # class, always the same set of functions is used for casting, so we are
  # triple safe here.
  # This test may require revision in case one of the codings used here gets
  # revised.
  # Information loss when casting into groups is signalled as a message (a
  # deliberate, intended aggregation), not as a warning.

  # Note that a cast where only one species_id from the original coding
  # translates in a goal coding which represents a group of species is NOT
  # considered backward ambiguous, because of the 1:1 match in the constellation
  # of the specific cast.

  expect_message(
    as_fe_species_tum_wwk_short(fe_species_ger_nfi_2012(c("211", "220"))),
    regexp = "^Cast loses information\\."
  )

  # (bavrn_state now codes the two oaks 110/111 separately, so that pair is no
  # longer ambiguous; two "other conifers" still collapse into bavrn_state 90.)
  expect_message(
    as_fe_species_bavrn_state(as_fe_species_master(c("abies_003", "abies_004"))),
    regexp = "^Cast loses information\\."
  )

  expect_message(
    as_fe_species_ger_nfi_2012(as_fe_species_bavrn_state(c("62", "88"))),
    regexp = "^Cast loses information\\."
  )

  expect_message(
    as_fe_species_bavrn_state_short(
      as_fe_species_master(c("larix_001", "larix_002"))
    ),
    regexp = "^Cast loses information\\."
  )

  expect_message(
    as_fe_species_tum_wwk_long(as_fe_species_ger_nfi_2012(c("110", "111"))),
    regexp = "^Cast loses information\\."
  )

  expect_message(
    as_fe_species_tum_wwk_short(
      as_fe_species_master(c("sorbus_001", "sorbus_002"))
    ),
    regexp = "^Cast loses information\\."
  )
})



test_that("a cast with no (complete) match in the goal coding errors", {
  # All shipped codings now cover every master species (tum_wwk_long used to be
  # the lone incomplete one, but it was completed). The "no complete match"
  # error path is therefore exercised here with a stubbed goal coding that
  # deliberately omits species: casting the full source into it must error.
  full       <- fe_species_get_coding_table("tum_wwk_short")
  incomplete <- head(full, 1) # a "goal" coding covering only one species

  testthat::local_mocked_bindings(
    fe_species_get_coding_table = function(coding) {
      if (identical(coding, "goal_incomplete")) incomplete else full
    }
  )

  expect_error(
    spec_id_cast_validate(full$species_id, "tum_wwk_short", "goal_incomplete"),
    regexp = "no \\(complete\\) match"
  )
})
