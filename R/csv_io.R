# license GPL-3
# This file is part of the R-package ForestElementsR.
#
# ForestElementsR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ForestElementsR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ForestElementsR.  If not, see <https://www.gnu.org/licenses/>.




# Internal CSV I/O helpers for the template/builder functions, robust to the
# realities of editing CSVs in a (German-locale) spreadsheet:
#
#  - write UTF-8 WITH a byte-order mark (so Excel shows umlauts correctly) and
#    a configurable field separator (";" by default, as German Excel expects);
#  - read back with automatic detection of the separator (";" vs ",") and of
#    the encoding: UTF-8 (with or without BOM), falling back to Windows-1252
#    if the bytes are not valid UTF-8 (i.e. Excel saved a plain "ANSI" CSV).
#
# All fields are written quoted; everything is read as character.


# Write a data frame as an Excel-friendly CSV (UTF-8 + BOM, CRLF, quoted)
.write_csv_excel <- function(df, file, sep = ";") {
  q <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
  }
  cols   <- lapply(df, q)
  header <- paste(q(names(df)), collapse = sep)
  body   <- if (nrow(df) > 0L) do.call(paste, c(cols, sep = sep)) else character(0)
  txt    <- paste0(paste(c(header, body), collapse = "\r\n"), "\r\n")

  con <- file(file, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con) # UTF-8 BOM
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(file)
}


# Read a CSV, auto-detecting separator and encoding; returns a character tibble
.read_csv_flexible <- function(file, sep = NULL) {
  raw <- readBin(file, "raw", n = file.size(file))

  if (length(raw) >= 3L &&
      all(raw[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) {
    raw <- raw[-(1:3)]
    s <- rawToChar(raw)
    Encoding(s) <- "UTF-8"
  } else {
    s <- rawToChar(raw)
    if (isTRUE(validUTF8(s))) {
      Encoding(s) <- "UTF-8"
    } else {
      s <- iconv(s, from = "WINDOWS-1252", to = "UTF-8")
    }
  }

  lines <- strsplit(s, "\r\n|\n|\r")[[1]]
  lines <- lines[!is.na(lines)]
  if (length(lines) == 0L) {
    return(tibble::tibble())
  }

  if (is.null(sep)) {
    h       <- lines[1]
    n_semi  <- nchar(gsub("[^;]", "", h))
    n_comma <- nchar(gsub("[^,]", "", h))
    sep     <- if (n_semi >= n_comma) ";" else ","
  }

  df <- utils::read.table(
    text             = paste(lines, collapse = "\n"),
    header           = TRUE,
    sep              = sep,
    quote            = "\"",
    colClasses       = "character",
    na.strings       = "",
    check.names      = FALSE,
    stringsAsFactors = FALSE,
    encoding         = "UTF-8",
    comment.char     = ""
  )
  tibble::as_tibble(df)
}
