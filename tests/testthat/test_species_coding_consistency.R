
library(ForestElementsR)


test_that("species master table and all coding tables are complete (no NA's)", {
  # species master table
  expect_identical(
    nrow(species_master_table), sum(complete.cases(species_master_table))
  )
  # coding master
  coding_table <- fe_species_get_coding_table("master")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
  # coding tum_wwk_short
  coding_table <- fe_species_get_coding_table("tum_wwk_short")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
  # coding ger_nfi_2012
  coding_table <- fe_species_get_coding_table("ger_nfi_2012")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
  # coding bavrn_state
  coding_table <- fe_species_get_coding_table("bavrn_state")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
  # coding bavrn_state_short
  coding_table <- fe_species_get_coding_table("bavrn_state_short")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
  # coding tum_wwk_long
  coding_table <- fe_species_get_coding_table("tum_wwk_long")
  expect_identical(
    nrow(coding_table), sum(complete.cases(coding_table))
  )
})



test_that("species_master_table is consistent", {
  n_distinct <- function(cols) {
    species_master_table |>
      dplyr::select(tidyselect::all_of(cols)) |>
      dplyr::distinct() |>
      nrow()
  }

  n <- nrow(species_master_table)

  # Is the key genus and species_no distinct?
  expect_identical(n_distinct(c("genus", "species_no")), n)
  # Is the scientific name column distinct?
  expect_identical(n_distinct(c("name_sci")), n)
  # Is the English name column distinct?
  expect_identical(n_distinct(c("name_eng")), n)
  # Is the German name column distinct?
  expect_identical(n_distinct(c("name_ger")), n)
  # Are there no missing values in the deciduous_conifer column?
  expect_true(all(!is.na(species_master_table$deciduous_conifer)))
  # Are there only the two values "decid", and "conif"
  expect_true(
    all(species_master_table$deciduous_conifer %in% c("decid", "conif"))
  )
})



test_that("each coding is in line with the species master", {
  # Helper function: Returns FALSE, if a given coding contains species that are
  # not listed in the species master table
  coding_consistent_with_master <- function(coding) {
    spcs_ctab <- fe_species_get_coding_table(coding)
    spec_ids <- spcs_ctab |>
      dplyr::select(genus, species_no, species_id) |>
      suppressMessages(dplyr::left_join(
        species_master_table |> dplyr::select(genus, species_no)
      )) |>
      purrr::pluck("species_id")

    all(!is.na(spec_ids))
  }

  expect_true(coding_consistent_with_master("master"))
  expect_true(coding_consistent_with_master("tum_wwk_short"))
  expect_true(coding_consistent_with_master("ger_nfi_2012"))
  expect_true(coding_consistent_with_master("bavrn_state"))
  expect_true(coding_consistent_with_master("bavrn_state_short"))
  expect_true(coding_consistent_with_master("tum_wwk_long"))
})



test_that("each coding's species names are distinct", {
  # Get the number of a coding's species_id's
  n_spec_ids <- function(coding) {
    fe_species_get_coding_table(coding) |>
      purrr::pluck("species_id") |>
      unique() |>
      length()
  }

  # Get the number of a coding's species names in the desired language
  n_names <- function(coding, name_column) {
    fe_species_get_coding_table(coding) |>
      dplyr::select(tidyselect::all_of(name_column)) |>
      purrr::pluck(1) |>
      unique() |>
      length()
  }

  # master
  coding <- "master"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)

  # tum_wwk_short
  coding <- "tum_wwk_short"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)

  # ger_nfi_2012
  coding <- "ger_nfi_2012"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)

  # bavrn_state
  coding <- "bavrn_state"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)

  # bavrn_state_short
  coding <- "bavrn_state_short"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)

  # tum_wwk_long
  coding <- "tum_wwk_long"
  n_spids <- n_spec_ids(coding)
  expect_identical(n_names(coding, "name_sci"), n_spids)
  expect_identical(n_names(coding, "name_eng"), n_spids)
  expect_identical(n_names(coding, "name_ger"), n_spids)
})



test_that("coding table 'master' and the species master table have same length", {
  # Consistency of both tables is checked above, but there could still be
  # species in the master table which are not in the coding. This must be
  # absolutely the case for the coding "master".
  # For the other codings a full coverage of all species in the master table is
  # not required
  expect_identical(
    species_master_table |> nrow(),
    fe_species_get_coding_table("master") |> nrow()
  )
})



test_that("nested codings convert without error from outer to inner", {

  # tum_wwk_long must always be convertible into tum_wwk_short
  outer <- fe_species_get_coding_table("tum_wwk_long")$species_id |>
    unique() |>
    as_fe_species_tum_wwk_long()

  # Nesting the expect_ was necessary because suppressing warnings as normal
  # made the test being skipped
  expect_warning( # Species cast warnings must come from casting into groups
    expect_no_error(
      as_fe_species_tum_wwk_short(outer)
    )
  )

  # bavrn_state must always be convertible into bavrn_state_short
  outer <- fe_species_get_coding_table("bavrn_state")$species_id |>
    unique() |>
    as_fe_species_bavrn_state()

  # Nesting the expect_ was necessary because suppressing warnings as normal
  # made the test being skipped
  expect_warning(  # Species cast warnings must come from casting into groups
    expect_no_error(
      as_fe_species_bavrn_state_short(outer)
    )
  )
})





