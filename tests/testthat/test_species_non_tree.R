
library(ForestElementsR)


test_that("non-tree codes are reported per coding", {
  # These codings consist of tree species only
  for (coding in c("master", "tum_wwk_short", "tum_wwk_long",
                   "ger_nfi_2012")) {
    expect_identical(fe_species_non_tree_codes(coding), character(0))
  }
  # bavrn_state carries the shrub code 99
  expect_identical(fe_species_non_tree_codes("bavrn_state"), "99")
})



test_that("fe_species_is_tree is all TRUE when a coding has no non-tree codes", {
  spec_ids <- fe_species_bavrn_state(c("10", "60", "80"))
  expect_identical(fe_species_is_tree(spec_ids), c(TRUE, TRUE, TRUE))
})



test_that("fe_species_is_tree returns NA for NA codes", {
  spec_ids <- fe_species_bavrn_state(c("10", NA, "60"))
  expect_identical(fe_species_is_tree(spec_ids), c(TRUE, NA, TRUE))
})



test_that("fe_species_non_tree_codes reads the is_tree column when present", {
  # Simulate a coding table that carries an is_tree flag (code 99 = non-tree)
  fake <- tibble::tribble(
    ~species_id, ~genus,        ~species_no,   ~is_tree,
    "10",        "picea",       "001",         TRUE,
    "80",        "tilia",       "001",         TRUE,
    "99",        NA_character_, NA_character_, FALSE
  )
  testthat::local_mocked_bindings(
    fe_species_get_coding_table = function(coding) fake
  )
  expect_identical(fe_species_non_tree_codes("whatever"), "99")
})



test_that(".assert_tree_species errors on non-tree codes only", {
  expect_no_error(
    .assert_tree_species(fe_species_bavrn_state(c("10", "60", "80")))
  )
  expect_error(
    .assert_tree_species(fe_species_bavrn_state(c("10", "99"))),
    regexp = "non-tree"
  )
})



test_that("fe_stand rejects non-tree species codes", {
  raw <- tibble::tibble(
    tree_no    = as.character(1:5),
    species_id = as_fe_species_bavrn_state(c("10", "10", "60", "60", "99")),
    time_yr    = 2022,
    dbh        = c(30, 32, 41, 38, 12)
  )
  # All-tree control builds fine
  expect_s3_class(
    fe_stand(raw[1:4, ], tree_id_col = "tree_no", species_id_col = "species_id",
             time_yr_col = "time_yr", dbh_cm_col = "dbh", area_ha = 0.5),
    "fe_stand"
  )
  # The shrub (code 99) is rejected
  expect_error(
    fe_stand(raw, tree_id_col = "tree_no", species_id_col = "species_id",
             time_yr_col = "time_yr", dbh_cm_col = "dbh", area_ha = 0.5),
    regexp = "non-tree"
  )
})



test_that("casting a non-tree code yields NA with a message", {
  expect_message(
    res <- as_fe_species_tum_wwk_short(fe_species_bavrn_state(c("10", "99"))),
    regexp = "Non-tree code"
  )
  v <- unclass(res)
  expect_false(is.na(v[[1]])) # tree code 10 converted
  expect_true(is.na(v[[2]]))  # non-tree code 99 -> NA
})
