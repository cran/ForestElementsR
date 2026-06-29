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




# CSV-driven (re)building of the species master table. The master table is the
# pure list of single species and the root of all codings; it therefore has a
# simpler format than a coding table (no species_id / parent_code / is_tree),
# and a dedicated, thin builder (the coding builder validates against the
# master and cannot bootstrap it).


# Canonical column set and order of the species master table
.master_table_cols <- c(
  "genus", "species_no", "deciduous_conifer", "name_sci", "name_eng", "name_ger"
)



#' Write the Species Master Table as a CSV Template
#'
#' Writes (or returns) the current \code{\link{species_master_table}} in its
#' canonical column order as a starting point for editing it (e.g. adding new
#' species) and rebuilding it with \code{\link{master_table_from_csv}}.
#'
#' @param file Optional path. If given, the master table is written there as a
#'   CSV; it is always returned (invisibly when written to a file). The CSV is
#'   written as UTF-8 with a byte-order mark (so umlauts display correctly in a
#'   spreadsheet) using \code{sep} as the field separator.
#'
#' @param sep Field separator used when writing to \code{file}. Defaults to
#'   \code{";"}, which is what a German-locale Excel expects; use \code{","}
#'   for a classic comma-separated file.
#'
#' @return A \code{tibble} with the columns \code{genus}, \code{species_no},
#'   \code{deciduous_conifer}, \code{name_sci}, \code{name_eng},
#'   \code{name_ger}
#'
#' @seealso \code{\link{master_table_from_csv}}
#'
#' @export
#'
#' @examples
#' master_template_csv() |> head()
#'
master_template_csv <- function(file = NULL, sep = ";") {
  mt <- ForestElementsR::species_master_table
  mt <- tibble::as_tibble(mt)[, .master_table_cols, drop = FALSE]

  if (!is.null(file)) {
    .write_csv_excel(mt, file, sep = sep)
    return(invisible(mt))
  }
  mt
}



#' Build the Species Master Table from a CSV
#'
#' Reads a pure species list (the master table's six columns and nothing else)
#' and turns it into a validated \code{\link{species_master_table}}. The master
#' table is the root of all codings, so this builder is deliberately strict and
#' fails early and clearly on any violation. Rows are sorted deterministically
#' by \code{deciduous_conifer}, then \code{genus}, then \code{species_no}.
#'
#' Checks performed:
#' \itemize{
#'   \item exactly the columns \code{genus}, \code{species_no},
#'     \code{deciduous_conifer}, \code{name_sci}, \code{name_eng},
#'     \code{name_ger} (extra or missing columns are an error),
#'   \item no missing values,
#'   \item \code{genus} all lower case,
#'   \item \code{species_no} a one- to three-digit number, normalised to three
#'     digits with leading zeros (\code{"1"}, \code{"01"}, \code{"001"} all
#'     become \code{"001"}),
#'   \item the key \code{(genus, species_no)} unique,
#'   \item \code{name_sci}, \code{name_eng}, \code{name_ger} each distinct,
#'   \item \code{deciduous_conifer} only \code{"conif"} or \code{"decid"}.
#' }
#'
#' @param template A \code{data.frame}/\code{tibble} holding the species list,
#'   or a path to a CSV file holding it. A CSV is read with automatic detection
#'   of the field separator (\code{";"} vs \code{","}) and of the encoding
#'   (UTF-8 with or without BOM, falling back to Windows-1252).
#'
#' @param sep Field separator to assume when \code{template} is a CSV file. By
#'   default (\code{NULL}) it is detected automatically from the file.
#'
#' @return A \code{tibble} suitable as \code{\link{species_master_table}}
#'
#' @seealso \code{\link{master_template_csv}}
#'
#' @export
#'
#' @examples
#' # Round trip: rebuild the current master table from its own template
#' mt <- master_table_from_csv(master_template_csv())
#' head(mt)
#'
master_table_from_csv <- function(template, sep = NULL) {
  if (is.character(template) && length(template) == 1L) {
    template <- .read_csv_flexible(template, sep = sep)
  }
  mt <- tibble::as_tibble(template)

  # --- column set ---------------------------------------------------------
  extra   <- setdiff(names(mt), .master_table_cols)
  missing <- setdiff(.master_table_cols, names(mt))
  if (length(extra) || length(missing)) {
    stop(
      "Master table CSV must contain exactly the columns ",
      paste(.master_table_cols, collapse = ", "), ".",
      if (length(missing)) paste0(" Missing: ", paste(missing, collapse = ", "), ".") else "",
      if (length(extra))   paste0(" Unexpected: ", paste(extra, collapse = ", "), ".") else "",
      call. = FALSE
    )
  }
  mt <- mt[, .master_table_cols, drop = FALSE]
  for (cc in .master_table_cols) mt[[cc]] <- trimws(as.character(mt[[cc]]))
  mt[mt == ""] <- NA

  # --- value checks (fail early, fail clearly) ----------------------------
  if (anyNA(mt)) {
    stop("Master table CSV must not contain missing values.", call. = FALSE)
  }
  bad_genus <- mt$genus != tolower(mt$genus)
  if (any(bad_genus)) {
    stop("genus must be lower case; offending: ",
         paste(unique(mt$genus[bad_genus]), collapse = ", "), call. = FALSE)
  }
  bad_no <- !grepl("^[0-9]{1,3}$", mt$species_no)
  if (any(bad_no)) {
    stop("species_no must be a one- to three-digit number; offending: ",
         paste(unique(mt$species_no[bad_no]), collapse = ", "), call. = FALSE)
  }
  # Normalise to three digits with leading zeros (so a spreadsheet turning
  # "001" into 1 is harmless)
  mt$species_no <- sprintf("%03d", as.integer(mt$species_no))
  dup_key <- duplicated(mt[c("genus", "species_no")]) |
    duplicated(mt[c("genus", "species_no")], fromLast = TRUE)
  if (any(dup_key)) {
    stop("(genus, species_no) must be unique; offending: ",
         paste(unique(paste(mt$genus[dup_key], mt$species_no[dup_key])),
               collapse = ", "), call. = FALSE)
  }
  for (nm in c("name_sci", "name_eng", "name_ger")) {
    d <- duplicated(mt[[nm]]) | duplicated(mt[[nm]], fromLast = TRUE)
    if (any(d)) {
      stop(nm, " must be distinct; offending: ",
           paste(unique(mt[[nm]][d]), collapse = ", "), call. = FALSE)
    }
  }
  bad_dc <- !(mt$deciduous_conifer %in% c("conif", "decid"))
  if (any(bad_dc)) {
    stop("deciduous_conifer must be 'conif' or 'decid'; offending: ",
         paste(unique(mt$deciduous_conifer[bad_dc]), collapse = ", "),
         call. = FALSE)
  }

  # --- deterministic order ------------------------------------------------
  # by deciduous_conifer (conif before decid), then genus, then species_no
  mt[order(mt$deciduous_conifer, mt$genus, mt$species_no), , drop = FALSE]
}
