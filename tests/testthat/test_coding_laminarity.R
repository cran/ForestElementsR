
library(ForestElementsR)


test_that("all current codings are laminar", {
  for (coding in c("master", "tum_wwk_short", "tum_wwk_long",
                   "ger_nfi_2012", "bavrn_state", "bavrn_state_short")) {
    ct <- fe_species_get_coding_table(coding)
    expect_no_error(.validate_coding_laminar(ct, coding))
  }
})



test_that("a valid laminar hierarchy with levels passes", {
  hier <- tibble::tribble(
    ~species_id, ~genus,    ~species_no, ~level,
    "54",        "quercus", "001",       0,
    "55",        "quercus", "002",       0,
    "70",        "quercus", "001",       1,
    "70",        "quercus", "002",       1,
    "71",        "quercus", "003",       0
  )
  expect_no_error(.validate_coding_laminar(hier, "synthetic"))
})



test_that("partially overlapping species-sets are rejected", {
  # Code A = {q001, q002}, code B = {q002, q003} -> partial overlap on q002
  bad <- tibble::tribble(
    ~species_id, ~genus,    ~species_no,
    "A",         "quercus", "001",
    "A",         "quercus", "002",
    "B",         "quercus", "002",
    "B",         "quercus", "003"
  )
  expect_error(.validate_coding_laminar(bad), regexp = "not laminar")
})



test_that("two codes with an identical species-set are rejected", {
  bad <- tibble::tribble(
    ~species_id, ~genus,    ~species_no,
    "A",         "quercus", "001",
    "B",         "quercus", "001"
  )
  expect_error(.validate_coding_laminar(bad), regexp = "identical species set")
})



test_that("level must increase along nesting", {
  # 70 (group, {q001,q002}) wrongly has a smaller level than its leaf 54
  bad <- tibble::tribble(
    ~species_id, ~genus,    ~species_no, ~level,
    "54",        "quercus", "001",       2,
    "70",        "quercus", "001",       1,
    "70",        "quercus", "002",       1
  )
  expect_error(.validate_coding_laminar(bad), regexp = "not smaller")
})



test_that("non-unique level for one code is rejected", {
  bad <- tibble::tribble(
    ~species_id, ~genus,    ~species_no, ~level,
    "70",        "quercus", "001",       1,
    "70",        "quercus", "002",       2
  )
  expect_error(.validate_coding_laminar(bad), regexp = "non-unique level")
})



test_that("rows without a master link are ignored", {
  # A non-tree code (NA master link) must not break laminarity
  with_nontree <- tibble::tribble(
    ~species_id, ~genus,        ~species_no,
    "10",        "picea",       "001",
    "99",        NA_character_, NA_character_
  )
  expect_no_error(.validate_coding_laminar(with_nontree))
})
