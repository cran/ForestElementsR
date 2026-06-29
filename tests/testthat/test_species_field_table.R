
library(ForestElementsR)


all_codings <- species_codings[["species_coding"]]


test_that("the field table has exactly one row per distinct code", {
  for (coding in all_codings) {
    ft <- fe_species_get_field_table(coding)
    ct <- fe_species_get_coding_table(coding)
    # one row per code, no code lost, no code duplicated
    expect_identical(nrow(ft), length(unique(ct[["species_id"]])))
    expect_false(any(duplicated(ft[["species_id"]])))
    expect_setequal(ft[["species_id"]], unique(ct[["species_id"]]))
  }
})



test_that("the field table carries all three name columns plus level/is_tree", {
  for (coding in all_codings) {
    ft <- fe_species_get_field_table(coding)
    expect_identical(
      names(ft),
      c("species_id", "name_sci", "name_eng", "name_ger", "level", "is_tree")
    )
    expect_type(ft[["level"]], "integer")
    expect_type(ft[["is_tree"]], "logical")
    # names are always present, independent of the language option
    expect_false(anyNA(ft[["name_sci"]]))
    expect_false(anyNA(ft[["name_eng"]]))
    expect_false(anyNA(ft[["name_ger"]]))
  }
})



test_that("names come from the coding, not from the master table", {
  # Group codes have no master-table entry; their names exist only in the
  # coding table. tum_wwk_short code 8 is the 'other hardwood' container group.
  ft <- fe_species_get_field_table("tum_wwk_short")
  grp <- ft[ft[["species_id"]] == "8", ]
  expect_identical(grp[["name_eng"]], "other hardwood")
  # The same names as in the coding table, not the master species names
  ct <- fe_species_get_coding_table("tum_wwk_short")
  ct8 <- unique(ct[ct[["species_id"]] == "8", c("name_sci", "name_eng",
                                                "name_ger")])
  expect_identical(
    as.data.frame(grp[, c("name_sci", "name_eng", "name_ger")]),
    as.data.frame(ct8)
  )
})



test_that("the field table is independent of the fe_spec_lang option", {
  opt_old <- getOption("fe_spec_lang")
  on.exit(options(fe_spec_lang = opt_old), add = TRUE)

  options(fe_spec_lang = "code")
  ft_code <- fe_species_get_field_table("bavrn_state")
  options(fe_spec_lang = "eng")
  ft_eng <- fe_species_get_field_table("bavrn_state")
  options(fe_spec_lang = NULL)
  ft_null <- fe_species_get_field_table("bavrn_state")

  expect_identical(ft_code, ft_eng)
  expect_identical(ft_code, ft_null)
})



test_that("non-tree codes appear in the field table flagged is_tree = FALSE", {
  ft <- fe_species_get_field_table("bavrn_state")
  shrub <- ft[ft[["species_id"]] == "99", ]
  expect_identical(nrow(shrub), 1L)
  expect_false(shrub[["is_tree"]])
  # all the other bavrn_state codes are tree codes
  expect_true(all(ft[["is_tree"]][ft[["species_id"]] != "99"]))
})



test_that("the field table is in canonical order (level, then code)", {
  for (coding in all_codings) {
    ft <- fe_species_get_field_table(coding)
    # level is non-decreasing (leaves before their groups)
    expect_false(is.unsorted(ft[["level"]]))
    # within each level, codes are sorted as the coding table stores them
    ct <- fe_species_get_coding_table(coding)
    canonical <- unique(ct[["species_id"]]) # coding table is canonical already
    expect_identical(ft[["species_id"]], canonical)
  }
})



test_that("the coding name can be taken from an fe_species object", {
  spec_ids <- fe_species_bavrn_state(c("10", "60"))
  coding   <- fe_species_get_coding(spec_ids)
  expect_identical(
    fe_species_get_field_table(coding),
    fe_species_get_field_table("bavrn_state")
  )
})
