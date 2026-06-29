
library(ForestElementsR)


test_that(".finest_per_species is a no-op for partition codings", {
  # For a partition coding (each master species in exactly one code) the helper
  # must return the table unchanged, row order included. Hierarchical codings
  # (e.g. bavrn_state and tum_wwk_long, where a species sits in both a leaf and
  # a group code) are deliberately not a no-op and are covered by the test below.
  for (coding in c("master", "tum_wwk_short", "ger_nfi_2012")) {
    ct <- fe_species_get_coding_table(coding)
    expect_identical(.finest_per_species(ct), ct)
  }
})



test_that(".finest_per_species keeps the finest code via the level column", {
  # Synthetic hierarchical oak coding: robur (q001) and petraea (q002) exist
  # both as leaf codes (54, 55, level 0) and inside the group 70 (level 1).
  hier <- tibble::tribble(
    ~species_id, ~genus,    ~species_no, ~level,
    "54",        "quercus", "001",       0,
    "55",        "quercus", "002",       0,
    "70",        "quercus", "001",       1,
    "70",        "quercus", "002",       1,
    "71",        "quercus", "003",       0
  )

  res <- .finest_per_species(hier)

  # One row per (genus, species_no)
  expect_identical(nrow(res), 3L)
  # The group code 70 is dropped in favour of the leaves
  expect_false("70" %in% res$species_id)
  # robur -> 54, petraea -> 55, rubra -> 71
  pick <- function(sp) res$species_id[res$species_no == sp]
  expect_identical(pick("001"), "54")
  expect_identical(pick("002"), "55")
  expect_identical(pick("003"), "71")
})



test_that(".finest_per_species falls back to species-set size without level", {
  # Same hierarchy, but no level column: finest = smallest species-set.
  # 54/55/71 cover one species each, 70 covers two -> leaves win.
  hier <- tibble::tribble(
    ~species_id, ~genus,    ~species_no,
    "54",        "quercus", "001",
    "55",        "quercus", "002",
    "70",        "quercus", "001",
    "70",        "quercus", "002",
    "71",        "quercus", "003"
  )

  res <- .finest_per_species(hier)

  expect_identical(nrow(res), 3L)
  expect_false("70" %in% res$species_id)
  expect_setequal(res$species_id, c("54", "55", "71"))
})
