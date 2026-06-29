
library(ForestElementsR)


test_that("declarative override resolves the forward-ambiguous 290 cast", {
  # ger_nfi_2012 "290" straddles tum_wwk_short 8 and 9 -> would be forward
  # ambiguous, but the registry resolves it to "8" with a message (not an
  # error, not a warning).
  x <- fe_species_ger_nfi_2012("290")

  expect_message(
    res <- as_fe_species_tum_wwk_short(x),
    regexp = "Applied cast override.*290 -> 8"
  )
  expect_true(is_fe_species_tum_wwk_short(res))
  expect_identical(unclass(res)[[1]], "8")
})



test_that("override leaves regular codes in the same vector untouched", {
  # "100" and "10" cast normally to tum_wwk_short ("5" and "1"); "290" is
  # resolved to "8" by the override; vector order is preserved.
  x <- fe_species_ger_nfi_2012(c("100", "290", "10"))

  res <- suppressMessages(as_fe_species_tum_wwk_short(x))
  expect_identical(unclass(res), c("5", "8", "1"))
})



test_that("non-overridden forward-ambiguous casts still error", {
  # tum_wwk_short "9" (a container group) has no override into "master" and
  # must still raise the ambiguity error. Goal "master" covers every species,
  # so this stays purely a forward-ambiguity case (no coverage gap).
  expect_error(
    as_fe_species_master(fe_species_tum_wwk_short("9")),
    regexp = "^Ambiguous cast attempt\\."
  )
})
