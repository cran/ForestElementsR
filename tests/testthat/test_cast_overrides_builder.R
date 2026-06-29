
library(ForestElementsR)


# Helper: a one-row override as an in-memory "CSV"
ov_row <- function(from = "ger_nfi_2012", to = "tum_wwk_short",
                   sid_from = "290", sid_to = "8") {
  tibble::tibble(coding_from = from, coding_to = to,
                 species_id_from = sid_from, species_id_to = sid_to)
}


test_that("the package override registry holds the documented cases", {
  expect_identical(
    names(species_cast_overrides),
    c("coding_from", "coding_to", "species_id_from", "species_id_to")
  )
  expect_identical(nrow(species_cast_overrides), 3L)
  expect_identical(
    species_cast_overrides$coding_from,
    c("ger_nfi_2012", "bavrn_state", "bavrn_state")
  )
  expect_identical(
    species_cast_overrides$coding_to,
    rep("tum_wwk_short", 3L)
  )
  expect_identical(species_cast_overrides$species_id_from, c("290", "70", "80"))
  expect_identical(species_cast_overrides$species_id_to, c("8", "6", "8"))
})



test_that("a valid override builds and keeps the four canonical columns", {
  ov <- cast_overrides_from_csv(ov_row())
  expect_identical(
    names(ov),
    c("coding_from", "coding_to", "species_id_from", "species_id_to")
  )
  expect_identical(nrow(ov), 1L)
})



test_that("structural problems are caught", {
  expect_error(cast_overrides_from_csv(tibble::tibble(coding_from = "x")),
               regexp = "missing column")
  expect_error(cast_overrides_from_csv(ov_row(sid_from = "")),
               regexp = "blank cells")
  dup <- dplyr::bind_rows(ov_row(sid_to = "8"), ov_row(sid_to = "9"))
  expect_error(cast_overrides_from_csv(dup), regexp = "duplicate")
})



test_that("semantic validation rejects invalid or unnecessary overrides", {
  # unknown coding
  expect_error(cast_overrides_from_csv(ov_row(from = "nope")),
               regexp = "unknown coding")
  # target code does not exist in the goal coding
  expect_error(cast_overrides_from_csv(ov_row(sid_to = "999")),
               regexp = "target code not in")
  # target exists but is not one of the straddled goal codes (290 -> {8, 9})
  expect_error(cast_overrides_from_csv(ov_row(sid_to = "6")),
               regexp = "straddled goal codes")
  # source cast is not forward-ambiguous (single-species code 10 -> 1)
  expect_error(
    cast_overrides_from_csv(ov_row(sid_from = "10", sid_to = "1")),
    regexp = "not forward-ambiguous"
  )
})



test_that("validate = FALSE skips the semantic checks", {
  ov <- cast_overrides_from_csv(ov_row(sid_from = "10", sid_to = "1"),
                                validate = FALSE)
  expect_identical(nrow(ov), 1L)
})
