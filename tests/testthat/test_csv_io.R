
library(ForestElementsR)


test_that("write/read round-trips umlauts with the default ';' separator", {
  df <- tibble::tibble(
    genus      = c("fagus", "tilia"),
    name_ger   = c("Buche", "Sommerlinde über öde ä ß"),
    species_no = c("001", "002")
  )
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  .write_csv_excel(df, f, sep = ";")
  back <- .read_csv_flexible(f)
  expect_identical(as.data.frame(back), as.data.frame(df))
})



test_that("the written file is UTF-8 with a BOM", {
  df <- tibble::tibble(a = "ä", b = "x")
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  .write_csv_excel(df, f)
  raw <- readBin(f, "raw", n = 3)
  expect_identical(raw, as.raw(c(0xEF, 0xBB, 0xBF)))
})



test_that("the separator is auto-detected (comma)", {
  df <- tibble::tibble(genus = "fagus", name_ger = "Buche")
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  .write_csv_excel(df, f, sep = ",")
  back <- .read_csv_flexible(f) # sep = NULL -> auto
  expect_identical(as.data.frame(back), as.data.frame(df))
})



test_that("a Windows-1252 (ANSI) file with umlauts is read correctly", {
  # Simulate what a plain German-Excel 'CSV' save produces: CP1252, ';'
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  txt <- "\"genus\";\"name_ger\"\r\n\"fagus\";\"Buche üöäß\"\r\n"
  bytes <- iconv(txt, from = "UTF-8", to = "WINDOWS-1252", toRaw = TRUE)[[1]]
  con <- file(f, open = "wb"); writeBin(bytes, con); close(con)
  back <- .read_csv_flexible(f)
  expect_identical(back$name_ger, "Buche üöäß")
})



test_that("master table round-trips through a written CSV file", {
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f))
  master_template_csv(f)
  rb <- master_table_from_csv(f)
  canon <- c("genus", "species_no", "deciduous_conifer",
             "name_sci", "name_eng", "name_ger")
  srt <- function(d) {
    d <- as.data.frame(d)[, canon, drop = FALSE]
    d <- d[order(d$genus, d$species_no), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  expect_equal(srt(rb), srt(species_master_table))
})
