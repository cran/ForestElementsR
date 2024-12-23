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
