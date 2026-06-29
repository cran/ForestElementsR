
library(ForestElementsR)


test_that("prefill template round-trips the partition codings", {
  # Partition codings (every species in exactly one code, no nesting) can be
  # reconstructed from their built table via the prefill template. Hierarchical
  # codings (e.g. bavrn_state) carry no parent_code in the built table, so they
  # are exercised through the synthetic templates below instead.
  shared <- c("species_id", "genus", "species_no", "deciduous_conifer",
              "name_sci", "name_eng", "name_ger")
  norm <- function(d) {
    d <- d[, shared, drop = FALSE]
    d <- d[order(d$species_id, d$genus, d$species_no), , drop = FALSE]
    rownames(d) <- NULL
    as.data.frame(d)
  }
  for (coding in c("master", "tum_wwk_short", "ger_nfi_2012")) {
    ct <- fe_species_get_coding_table(coding)
    rb <- coding_table_from_template(coding_template_from_master(coding), coding)
    expect_equal(norm(rb), norm(ct), info = coding)
  }
})



test_that("empty template has one row per master species and no is_tree column", {
  tmpl <- coding_template_from_master()
  expect_identical(nrow(tmpl), nrow(species_master_table))
  expect_true(all(is.na(tmpl$species_id)))
  # is_tree is derived at build time, not part of the editable template
  expect_false("is_tree" %in% names(tmpl))
})



test_that("builder resolves a hierarchical coding and derives a non-tree code", {
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng,        ~name_ger,
    "54",        "70",         "quercus", "001",       NA,              NA,               NA,
    "55",        "70",         "quercus", "002",       NA,              NA,               NA,
    "71",        NA,           "quercus", "003",       NA,              NA,               NA,
    "90",        NA,           "picea",   "001",       NA,              NA,               NA,
    "90",        NA,           "abies",   "001",       NA,              NA,               NA,
    "70",        NA,           NA,        NA,          "Quercus spec.", "oak",            "Eiche",
    "90",        NA,           NA,        NA,          "alia conifera", "other conifers", "Sonstiges Nadelholz",
    "99",        NA,           NA,        NA,          "frutex",        "shrub",          "Strauch"
  )
  # The unlinkable code 99 is reported as a non-tree category
  expect_message(
    ct <- coding_table_from_template(tmpl, "synthetic"),
    "without a master link, treated as non-tree: 99 \\(Strauch\\)"
  )

  # The group 70 carries both oak species and sits one level above its leaves
  oak70 <- ct[ct$species_id == "70", ]
  expect_setequal(oak70$species_no, c("001", "002"))
  expect_true(all(ct$level[ct$species_id == "70"] == 1))
  expect_true(all(ct$level[ct$species_id %in% c("54", "55", "71", "90")] == 0))

  # Leaf 54 inherits its name from the master table
  expect_identical(unique(ct$name_ger[ct$species_id == "54"]), "Stieleiche")
  # Group 70 uses the declared name
  expect_identical(unique(ct$name_ger[ct$species_id == "70"]), "Eiche")

  # is_tree is derived: group/leaf codes with master links are trees ...
  expect_true(all(ct$is_tree[ct$species_id %in% c("54", "55", "70", "90")]))
  # ... and the code with no master link is the only non-tree code
  expect_setequal(ct$species_id[!ct$is_tree], "99")
  row99 <- ct[ct$species_id == "99", ]
  expect_identical(nrow(row99), 1L)
  expect_true(is.na(row99$genus))
  expect_identical(row99$name_ger, "Strauch")
})



test_that("builder derives multi-level nesting depth", {
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng,   ~name_ger,
    "54",        "70",         "quercus", "001",       NA,              NA,          NA,
    "55",        "70",         "quercus", "002",       NA,              NA,          NA,
    "71",        "100",        "quercus", "003",       NA,              NA,          NA,
    "70",        "100",        NA,        NA,          "Quercus spec.", "oak",       "Eiche",
    "100",       NA,           NA,        NA,          "Latifoliae",    "broadleaf", "Laubholz"
  )
  ct <- coding_table_from_template(tmpl, "synthetic")
  lvl <- function(id) unique(ct$level[ct$species_id == id])
  expect_identical(lvl("54"), 0L)
  expect_identical(lvl("70"), 1L)
  expect_identical(lvl("100"), 2L)
})



test_that("a stray is_tree column in the input is ignored (is_tree is derived)", {
  # Older CSVs still carry an is_tree column. It must be ignored: is_tree comes
  # from the master link, so a group code wrongly marked FALSE stays a tree.
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~is_tree, ~name_sci,       ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "001",       "FALSE",  NA,              NA,        NA,
    "55",        "70",         "quercus", "002",       "FALSE",  NA,              NA,        NA,
    "70",        NA,           NA,        NA,          "FALSE",  "Quercus spec.", "oak",     "Eiche"
  )
  ct <- coding_table_from_template(tmpl, "synthetic")
  # All codes have master links -> all trees, regardless of the stray column
  expect_true(all(ct$is_tree))
})



test_that("a group code without any name is an error", {
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_ger,
    "70",        NA,           "quercus", "001",       NA,
    "70",        NA,           "quercus", "002",       NA
  )
  expect_error(coding_table_from_template(tmpl),
               regexp = "all three languages")
})



test_that("a code with an incomplete set of language names is an error", {
  # Group 70 has only a German name (sci/eng missing) -> must error, else
  # format() would later return NA for those languages (the Tilia spec. case).
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci, ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "001",       NA,        NA,        NA,
    "55",        "70",         "quercus", "002",       NA,        NA,        NA,
    "70",        NA,           NA,        NA,          NA,        NA,        "Eiche"
  )
  expect_error(coding_table_from_template(tmpl),
               regexp = "all three languages")
})



test_that("builder normalises stripped species_no (Excel-proof)", {
  # Excel turned the leading-zero species_no into bare integers
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "1",         NA,              NA,        NA,
    "55",        "70",         "quercus", "2",         NA,              NA,        NA,
    "70",        NA,           NA,        NA,          "Quercus spec.", "oak",     "Eiche"
  )
  ct <- coding_table_from_template(tmpl, "synthetic")
  expect_true(all(grepl("^[0-9]{3}$", ct$species_no)))
  expect_setequal(ct$species_no[ct$species_id == "70"], c("001", "002"))
  # Names still resolved from master via the normalised key
  expect_identical(unique(ct$name_ger[ct$species_id == "54"]), "Stieleiche")
})



test_that("coding tables are stored in canonical order", {
  # Contract: rows ordered by level (ascending, leaves before groups), then the
  # code, then the master key. Tested once here so the functional tests need not
  # depend on row order. Master is unaffected (all level 0, non-numeric codes).
  for (coding in c("master", "tum_wwk_short", "tum_wwk_long", "ger_nfi_2012",
                   "bavrn_state", "bavrn_state_short")) {
    ct     <- fe_species_get_coding_table(coding)
    sorted <- dplyr::arrange(ct, level,
                             suppressWarnings(as.numeric(species_id)),
                             genus, species_no)
    expect_equal(ct, sorted, info = coding)
  }
})



test_that("conflicting names within one code are rejected", {
  # Code 80 gets two different German display names on its two rows -> error
  # (the builder must not silently keep the first one).
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,   ~species_no, ~name_sci,        ~name_eng,        ~name_ger,
    "80",        NA,           "carya",  "001",       "aliae deciduae", "other hardwood", "Other broadleaves",
    "80",        NA,           "sorbus", "004",       "aliae deciduae", "other hardwood", "Noble broadleaves"
  )
  expect_error(coding_table_from_template(tmpl, "synthetic"),
               regexp = "conflicting name_ger")
})



test_that("a concrete species not in the master table is a hard error", {
  # Option (b): a row naming a concrete but unknown master species is a typo,
  # caught hard - it does not silently degrade to a non-tree code.
  tmpl <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_ger,
    "1",         NA,           "quercus", "999",       NA
  )
  expect_error(coding_table_from_template(tmpl), regexp = "not in master")
})



# --- aggregation helper (stage 1 hints + stage 2 coarsening validator) -------

test_that("the aggregation registry maps short codings to their parent", {
  expect_identical(.aggregate_parent_of("bavrn_state_short"), "bavrn_state")
  expect_identical(.aggregate_parent_of("tum_wwk_short"), "tum_wwk_long")
  expect_true(is.na(.aggregate_parent_of("master")))
  expect_true(is.na(.aggregate_parent_of(NULL)))
})



test_that("aggregate_of adds parent-group hint columns and clusters them", {
  # bavrn_state_short aggregates bavrn_state (known from the registry)
  tmpl <- coding_template_from_master("bavrn_state_short")
  expect_true(all(c("agg_from_id", "agg_from_ger") %in% names(tmpl)))

  # The oaks bavrn_state puts under group 70 all share that coarsest code, so
  # they must end up with the same short code - the editing hint.
  oaks70 <- tmpl[tmpl$genus %in% "quercus" & tmpl$species_no %in%
                   c("001", "002", "003", "006"), ]
  expect_true(all(oaks70$agg_from_id == "70"))
  expect_true(all(oaks70$agg_from_ger == unique(oaks70$agg_from_ger)))

  # Hint columns are reference-only: building ignores them
  expect_false("agg_from_id" %in%
                 names(coding_table_from_template(
                   coding_template_from_master("tum_wwk_short"), "tum_wwk_short"
                 )))
})



test_that("coding_table_from_template accepts a valid aggregation", {
  parent <- coding_table_from_template(tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "001",       NA,              NA,        NA,
    "55",        "70",         "quercus", "002",       NA,              NA,        NA,
    "70",        NA,           NA,        NA,          "Quercus spec.", "oak",     "Eiche"
  ), "parent")

  # Both oaks roll up into one short code 7 -> valid coarsening
  child_ok <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng, ~name_ger,
    "7",         NA,           "quercus", "001",       NA,              NA,        NA,
    "7",         NA,           "quercus", "002",       "Quercus spec.", "oak",     "Eiche"
  )
  expect_s3_class(
    coding_table_from_template(child_ok, "child", parent_table = parent),
    "tbl_df"
  )
})



test_that("a parent group split across short codes is rejected", {
  parent <- coding_table_from_template(tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "001",       NA,              NA,        NA,
    "55",        "70",         "quercus", "002",       NA,              NA,        NA,
    "70",        NA,           NA,        NA,          "Quercus spec.", "oak",     "Eiche"
  ), "parent")

  # The two members of group 70 land in different short codes -> cast would break
  child_bad <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_ger,
    "7",         NA,           "quercus", "001",       NA,
    "8",         NA,           "quercus", "002",       NA
  )
  expect_error(
    coding_table_from_template(child_bad, "child", parent_table = parent,
                               parent_name = "parent"),
    regexp = "not a valid aggregation"
  )
})



test_that("a parent species not covered by the short coding is rejected", {
  parent <- coding_table_from_template(tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_sci,       ~name_eng, ~name_ger,
    "54",        "70",         "quercus", "001",       NA,              NA,        NA,
    "55",        "70",         "quercus", "002",       NA,              NA,        NA,
    "70",        NA,           NA,        NA,          "Quercus spec.", "oak",     "Eiche"
  ), "parent")

  # Only one of the two parent species is covered -> incomplete coarsening
  child_miss <- tibble::tribble(
    ~species_id, ~parent_code, ~genus,    ~species_no, ~name_ger,
    "7",         NA,           "quercus", "001",       NA
  )
  expect_error(
    coding_table_from_template(child_miss, "child", parent_table = parent),
    regexp = "not covered"
  )
})
