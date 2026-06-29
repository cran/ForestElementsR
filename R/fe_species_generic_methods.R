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




# Generic methods (including constructur validator and helper) for all
# fe_species classes

# As all fe_species_id classes are implemented as vctrs, no superclasses with
# common properties can be built. This is the reason for this workaround which
# provides generic functions fo an fe_species object's core functionality



#' Generic Constructor for **fe_species** Objects
#'
#' @param x A \code{character} vector
#'
#' @param class_name Name of the class to be created. Must be one of the
#'   supported names (first column of data \code{species_codings} with the
#'   prefix "fe_species_")
#'
#' @return An object of class \code{class_name} if x is \code{character} and
#'   \code{class_name} is among the supported names. Otherwise, the function
#'   terminates with an error.
#'
#' @keywords internal
#'
new_fe_species <- function(x = character(), class_name) {
  allowed_class_names <- paste0(
    "fe_species_",
    ForestElementsR::species_codings |>
      purrr::pluck("species_coding")
  )

  if (!(class_name %in% allowed_class_names)) {
    stop(
      paste0(
        "Class name '", class_name, "' is not supported for fe_species objects"
      ),
      call. = FALSE
    )
  }

  vctrs::vec_assert(x, character())
  vctrs::new_vctr(x, class = class_name)
}



#' Get Name of the Coding Belonging to an **fe_species** Object
#'
#' @param x An object of one of the supported \code{fe_species} classes
#'
#' @return The requested coding name (\code{character})
#'
#' @export
#'
#' @examples
#' spec_ids <- fe_species_ger_nfi_2012(c("10", "10", "30"))
#' fe_species_get_coding(spec_ids)
#'
fe_species_get_coding <- function(x) {
  class_name <- attr(x, "class")
  class_name <- class_name[grep("^fe_species_", class_name)]
  stringr::str_remove(class_name, "^fe_species_")
}



#' Get the Coding Table of a Supported **fe_species** Coding
#'
#' @param coding A character string representing one of the supported codings as
#'   can be requested for a given **fe_species** object with
#'   \code{\link{fe_species_get_coding}}, or one of the entries in the column
#'   \code{species_coding} of the data \code{species_codings}
#'
#' @return A \code{data.frame} (\code{tibble}) representing the requested coding
#'
#' @export
#'
#' @examples
#' fe_species_get_coding_table("ger_nfi_2012")
#' fe_species_get_coding_table("bavrn_state")
#' fe_species_get_coding_table("tum_wwk_short")
#'
fe_species_get_coding_table <- function(coding) {
  ForestElementsR::species_codings |>
    dplyr::filter(.data$species_coding == coding) |>
    purrr::pluck(2, 1)
}



#' Non-Tree Codes of a Species Coding
#'
#' Some species codings contain legal codes that do not correspond to a tree
#' species and cannot be used in calculations (e.g. a "shrub" category). Such
#' codes are flagged in the coding table with \code{is_tree = FALSE}. This
#' function returns them for a given coding. If the coding table has no
#' \code{is_tree} column (i.e. all of its codes denote tree species), an empty
#' character vector is returned.
#'
#' @param coding A character string naming one of the supported codings (see
#'   the data \code{\link{species_codings}})
#'
#' @return A \code{character} vector of the coding's non-tree \code{species_id}s
#'   (empty if there are none)
#'
#' @export
#'
#' @examples
#' fe_species_non_tree_codes("bavrn_state")
#'
fe_species_non_tree_codes <- function(coding) {
  ct <- fe_species_get_coding_table(coding)
  if (!("is_tree" %in% names(ct))) {
    return(character(0))
  }
  unique(ct$species_id[!ct$is_tree])
}



#' Test Which Elements of a Species Code Vector Denote Tree Species
#'
#' Returns a logical vector indicating, for each element of an **fe_species**
#' vector, whether its code denotes a tree species (\code{TRUE}) or a non-tree
#' category such as a shrub (\code{FALSE}; see
#' \code{\link{fe_species_non_tree_codes}}). \code{NA} codes yield \code{NA}.
#'
#' @param x An object of one of the supported \code{fe_species} classes
#'
#' @return A \code{logical} vector of the same length as \code{x}
#'
#' @export
#'
#' @examples
#' spec_ids <- fe_species_bavrn_state(c("10", "60", "80"))
#' fe_species_is_tree(spec_ids)
#'
fe_species_is_tree <- function(x) {
  coding   <- fe_species_get_coding(x)
  non_tree <- fe_species_non_tree_codes(coding)
  v   <- vctrs::vec_data(x)
  res <- !(v %in% non_tree)
  res[is.na(v)] <- NA
  res
}



#' Field Lookup Table of a Species Coding
#'
#' Produces a compact, field-usable lookup table for a species coding: **each
#' code appears exactly once**, accompanied by the name of the species (or
#' species group) it stands for. This is the view one would print as a coding
#' key for field inventory work.
#'
#' This differs from the full coding table returned by
#' \code{\link{fe_species_get_coding_table}}, which carries **one row per
#' elementary species** (so a group code shows up several times, once per member
#' species, and so do the leaf codes of a hierarchical coding). The field table
#' is the view collapsed to the level of distinct codes.
#'
#' The names are taken from the coding itself (the \code{name_*} columns of the
#' coding table), **not** from the \code{\link{species_master_table}}. This
#' matters for group codes, which have no master-table entry, and for codings
#' that use regional or otherwise coding-specific names. All three name columns
#' (\code{name_sci}, \code{name_eng}, \code{name_ger}) are always returned,
#' independent of the current \code{options("fe_spec_lang")} setting, so the
#' table is ready for output in any language.
#'
#' The rows are returned in the coding's canonical order (by \code{level}, then
#' by code), so the finest (leaf) codes come first and the coarser group codes
#' follow. The columns \code{level} (0 = finest leaf, higher = coarser group)
#' and \code{is_tree} (\code{FALSE} for non-tree categories such as a "shrub"
#' code) are kept, as both are relevant in the field.
#'
#' Rendering this table into a nicely formatted, printable document (e.g. a PDF)
#' is deliberately left to the downstream packages that already carry a document
#' rendering toolchain (\pkg{FeNEU}, \pkg{FeNEUShiny}); \pkg{ForestElementsR}
#' only provides the data.
#'
#' @param coding A character string naming one of the supported codings (see the
#'   data \code{\link{species_codings}}), or the coding name obtained from an
#'   existing **fe_species** object via \code{\link{fe_species_get_coding}}
#'
#' @return A \code{tibble} with one row per distinct code and the columns
#'   \code{species_id}, \code{name_sci}, \code{name_eng}, \code{name_ger},
#'   \code{level}, and \code{is_tree}
#'
#' @seealso \code{\link{fe_species_get_coding_table}},
#'   \code{\link{fe_species_non_tree_codes}}
#'
#' @export
#'
#' @examples
#' # Field key for the Bavarian state forest coding
#' fe_species_get_field_table("bavrn_state")
#'
#' # Works for every implemented coding
#' fe_species_get_field_table("tum_wwk_short")
#'
fe_species_get_field_table <- function(coding) {
  ct <- fe_species_get_coding_table(coding)

  # The coding table is stored in canonical order and the builder guarantees
  # that the names, level, and is_tree are constant within a code, so keeping
  # the first row per species_id yields the canonical, one-row-per-code view.
  # The name_* columns come from the coding table (i.e. the coding's own names,
  # including group names), never from the species master table.
  ct |>
    dplyr::distinct(.data$species_id, .keep_all = TRUE) |>
    dplyr::select(
      "species_id",
      "name_sci", "name_eng", "name_ger",
      "level", "is_tree"
    )
}



#' Generic Formatted Output for **fe_species** Objects
#'
#' @param x Object of one of the supported \code{fe_species_} classes
#'
#' @param spec_lang Choice of how species (group) names or id's are displayed.
#'   Supported choices are "code" (default, displays the species codes as they
#'   are), "eng" (English species names), "ger" (German species names), and
#'   "sci" (scientific species names). The names and the codes refer to the
#'   species coding given in the object's attribute \code{species_coding}.
#'
#' @return  A \code{character} vector either displaying the original species
#'   codes provided in \code{x}, or the species (group) names in the desired
#'   language
#'
#' @keywords internal
#'
format_fe_species <- function(x, spec_lang = c("code", "eng", "ger", "sci")) {
  out <- vctrs::vec_data(x)

  # Apply desired species naming
  spec_lang <- match.arg(spec_lang)

  if (spec_lang != "code") {
    coding <- fe_species_get_coding(x)
    out <- translate_spec_codes_into_names(out, spec_lang, coding)
  }

  out
}



#' Generic Validator for **fe_species** Objects
#'
#' @param x An object that is expected to be a correct instance of one of the
#'   supported \code{fe_species} classes
#'
#' @return Returns \code{x} if it passes the validation, terminates with an
#'   error otherwise
#'
#' @keywords internal
#'
validate_fe_species <- function(x) {
  # Given codes must be supported by the chosen coding
  coding <- fe_species_get_coding(x)
  check_rslt <- all_codes_allowed(x, coding)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste0(
        check_rslt$err_message,
        " by species coding '", coding, "'"
      ),
      call. = FALSE
    )
  }

  x
}





#' Generic Summary for **fe_species** Objects
#'
#' Produces a summary for a fe_species object in the same style as R does
#' factors. Actually, after some conversions \code{\link{summary.factor}} *is*
#' called by this function. The species naming in the summary depends on the
#' parameter \code{spec_lang}.
#'
#' @param x Object of one of the supported \code{fe_species_} classes
#'
#' @param spec_lang Choice of how species (group) names or id's are displayed.
#'   Supported choices are "code" (default, displays the species codes as they
#'   are), "eng" (English species names), "ger" (German species names), and
#'   "sci" (scientific species names). The names and the codes refer to the
#'   species coding given in the object's attribute \code{species_coding}.
#'
#' @param maxsum Same as parameter \code{maxsum} in \code{\link{summary.factor}}
#'
#' @return A named vector in the same style as returned by
#'   \code{\link{summary.factor}}
#'
#' @keywords internal
#'
summary_fe_species <- function(x,
                               spec_lang = c("code", "eng", "ger", "sci"),
                               maxsum = 100L) {
  # Transform into character
  x_char <- vctrs::vec_data(x)

  # Apply desired species naming
  spec_lang <- match.arg(spec_lang)

  if (spec_lang != "code") {
    coding <- fe_species_get_coding(x)
    x_char <- translate_spec_codes_into_names(x_char, spec_lang, coding)
  }

  # Transform into a factor
  x_fac <- as.factor(x_char)

  # Apply the summary method for factors
  summary(x_fac, maxsum)
}
