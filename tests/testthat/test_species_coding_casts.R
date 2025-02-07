
library(ForestElementsR)


test_that("forward ambiguous casts terminate with an error", {
  # Six ambiguous casts. Independent of the specific fe_species class, always
  # the same set of functions is used for casting, so we are triple safe here.
  # This test may require revision in case one of the codings used here gets
  # revised.

  expect_error(
    as_fe_species_ger_nfi_2012(fe_species_tum_wwk_short("9")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_ger_nfi_2012(fe_species_bavrn_state("70")),
    regexp = "^Ambiguous cast attempt\\."
  )

  expect_error(
    as_fe_species_bavrn_state(fe_species_tum_wwk_short("8")),
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
})



test_that("backward ambiguous casts raise a warning", {
  # Six backward ambiguous casts. Independent of the specific fe_species
  # class, always the same set of functions is used for casting, so we are
  # triple safe here.
  # This test may require revision in case one of the codings used here gets
  # revised.

  # Note that a cast where only one species_id from the original coding
  # translates in a goal coding which represents a group of species is NOT
  # considered backward ambiguous, because of the 1:1 match in the constellation
  # of the specific cast.

  expect_warning(
    as_fe_species_tum_wwk_short(fe_species_ger_nfi_2012(c("211", "220"))),
    regexp = "^Cast loses information\\."
  )

  expect_warning(
    as_fe_species_bavrn_state(fe_species_ger_nfi_2012(c("110", "111"))),
    regexp = "^Cast loses information\\."
  )

  expect_warning(
    as_fe_species_ger_nfi_2012(as_fe_species_bavrn_state(c("62", "88"))),
    regexp = "^Cast loses information\\."
  )

  expect_warning(
    as_fe_species_bavrn_state_short(
      as_fe_species_master(c("larix_001", "larix_002"))
    ),
    regexp = "^Cast loses information\\."
  )

  expect_warning(
    as_fe_species_tum_wwk_long(as_fe_species_ger_nfi_2012(c("110", "111"))),
    regexp = "^Cast loses information\\."
  )

  expect_warning(
    as_fe_species_tum_wwk_short(
      as_fe_species_master(c("sorbus_001", "sorbus_002"))
    ),
    regexp = "^Cast loses information\\."
  )
})



test_that("casts with no (complete) matches terminate with an error", {
  # So far, the species coding tum_wwk_long is the only one which does not
  # completely cover all allowed species. Therefore, species coding casts might
  # not always have (complete) matches in the goal coding, which must raise
  # an error

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_ger_nfi_2012(c("29", "24"))),
    regexp = "^Original code\\(s\\)"
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_bavrn_state(c("90"))),
    regexp = "^Original code\\(s\\)"
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_bavrn_state_short("9")),
    regexp = "^Original code\\(s\\)"
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_tum_wwk_short(c("9"))),
    regexp = "^Original code\\(s\\)"
  )

  expect_error(
    as_fe_species_tum_wwk_long(fe_species_master("pinus_007")),
    regexp = "^Original code\\(s\\)"
  )
})
