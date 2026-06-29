
library(ForestElementsR)


canon <- c("genus", "species_no", "deciduous_conifer",
           "name_sci", "name_eng", "name_ger")
sort_master <- function(d) {
  d <- as.data.frame(d)[, canon, drop = FALSE]
  d <- d[order(d$genus, d$species_no), , drop = FALSE]
  rownames(d) <- NULL
  d
}


test_that("master template -> build round-trips the master table", {
  rb <- master_table_from_csv(master_template_csv())
  expect_equal(sort_master(rb), sort_master(species_master_table))
})



test_that("a new species can be added and is sorted in deterministically", {
  # Use a clearly fictitious species so the test stays valid no matter which
  # real species the master table gains over time.
  tmpl <- master_template_csv()
  tmpl <- dplyr::bind_rows(
    tmpl,
    tibble::tibble(
      genus = "xzzgenus", species_no = "001", deciduous_conifer = "conif",
      name_sci = "Xzzgenus testus", name_eng = "test fir xzz",
      name_ger = "Testtanne xzz"
    )
  )
  rb <- master_table_from_csv(tmpl)
  expect_true(any(rb$genus == "xzzgenus" & rb$species_no == "001"))
  # deterministic order: deciduous_conifer, then genus, then species_no
  expect_identical(
    seq_len(nrow(rb)),
    order(rb$deciduous_conifer, rb$genus, rb$species_no)
  )
})



test_that("unexpected or missing columns are rejected", {
  tmpl <- master_template_csv()
  tmpl$species_id <- "x"
  expect_error(master_table_from_csv(tmpl), regexp = "exactly the columns")
})



test_that("missing values are rejected", {
  tmpl <- master_template_csv()
  tmpl$name_ger[1] <- NA
  expect_error(master_table_from_csv(tmpl), regexp = "missing values")
})



test_that("malformed species_no is rejected", {
  tmpl <- master_template_csv()
  tmpl$species_no[1] <- "12x"
  expect_error(master_table_from_csv(tmpl), regexp = "one- to three-digit")
})



test_that("species_no is normalised to three digits (Excel-proof)", {
  tmpl <- master_template_csv()
  # Simulate Excel stripping leading zeros on the abies block
  ab <- tmpl$genus == "abies"
  tmpl$species_no[ab] <- as.character(as.integer(tmpl$species_no[ab]))
  expect_true(any(tmpl$species_no[ab] == "1")) # really stripped
  rb <- master_table_from_csv(tmpl)
  # All abies species_no back to three-digit form, key intact
  expect_true(all(grepl("^[0-9]{3}$", rb$species_no)))
  expect_true(any(rb$genus == "abies" & rb$species_no == "001"))
})



test_that("a duplicate (genus, species_no) key is rejected", {
  tmpl <- master_template_csv()
  dup <- tmpl[1, ]
  dup$name_sci <- "Xxx yyy"; dup$name_eng <- "xxx"; dup$name_ger <- "Xxx"
  expect_error(master_table_from_csv(dplyr::bind_rows(tmpl, dup)),
               regexp = "must be unique")
})



test_that("a duplicate species name is rejected", {
  tmpl <- master_template_csv()
  dup <- tmpl[1, ]
  dup$genus <- "xxxgenus"; dup$species_no <- "999"
  # keep dup$name_* identical to row 1 -> name clash
  expect_error(master_table_from_csv(dplyr::bind_rows(tmpl, dup)),
               regexp = "must be distinct")
})



test_that("an invalid deciduous_conifer value is rejected", {
  tmpl <- master_template_csv()
  tmpl$deciduous_conifer[1] <- "tree"
  expect_error(master_table_from_csv(tmpl), regexp = "conif")
})
