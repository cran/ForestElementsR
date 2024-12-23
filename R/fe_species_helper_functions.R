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




# Functions intended for internal use in validators and type casting for species
# code objects intended for covering typical check situations


#' Check Species Codes for Compliance with a Given Coding
#'
#' @param x Object of one of the fe_species_id classes or a \code{character}
#'   vector
#'
#' @param coding Name of the coding against which \code{x} is to be checked. Must
#'   be one of the entries in the column \code{species_coding} in the data file
#'   \code{\link{species_codings}}
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements of x)
#'
#' @keywords internal
#'
all_codes_allowed <- function(x, coding) {
  x <- unique(stats::na.omit(x))

  allowed <- fe_species_get_coding_table(coding) |>
    purrr::pluck("species_id") |>
    unique()

  rslt <- vctrs::vec_data(x) %in% allowed
  rqmt_ok <- all(rslt)
  err_message <- paste(
    "Code(s)",
    paste(x[!rslt], collapse = ", "),
    "is/are not supported"
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message
  )
}




#' Translate Species Codes into Names
#'
#' @param x Object of one of the fe_species_id classes or a \code{character}
#'   vector
#'
#' @param spec_lang Choice of how species (group) names or id's are translated.
#'   Supported choices are "eng" (English species names, default), "ger" (Geman
#'   species names), and "sci" (scientific species names).
#'
#' @param coding Name of the coding against which \code{x} is to be checked.
#'   Must be one of the entries in the column \code{species_coding} in the data
#'   file \code{\link{species_codings}}
#'
#' @return A \code{character} vector in the same order as \code{x}, containing
#'   the species (group names) in the desired language
#'
#' @keywords internal
#'
translate_spec_codes_into_names <- function(x,
                                            spec_lang = c("eng", "ger", "sci"),
                                            coding) {
  col_head <- dplyr::case_when(
    spec_lang == "eng" ~ "name_eng",
    spec_lang == "ger" ~ "name_ger",
    spec_lang == "sci" ~ "name_sci",
  )

  # select correct species coding table
  species_tab <- fe_species_get_coding_table(coding)

  # make translation vector
  trans_vec <- species_tab[[col_head]]
  names(trans_vec) <- species_tab[["species_id"]]

  # translate
  rslt <- trans_vec[x]
  names(rslt) <- NULL

  rslt
}




#' Check an Intended Species Coding Cast for Complete Matches in the Goal Coding
#'
#' This function is happy if each code in the original species coding has a
#' complete match in the goal coding. It is not happy if any species from the
#' original coding has no counterpart in the goal coding.It is also not happy if
#' a species group from the original coding is not completely contained or equal
#' to a species group in the goal coding.
#'
#' @param x Vector of species codes (\code{character})
#'
#' @param coding_from Original species coding name (character)
#'
#' @param coding_to Goal coding name (character)
#'
#' @return A list with three elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements of x), 3)
#'   \code{no_match_table}, a data.frame showing the original species codes that
#'   have no match in the goal coding (together with their genus and species_no
#'   from the \code{\link{species_master_table}})
#'
#' @noRd
#'
#' @examples
#' ForestElementsR:::spec_id_cast_all_specs_in_goal_coding(
#'   c("29", "10", "24"), "ger_nfi_2012", "tum_wwk_long"
#' )
#'
spec_id_cast_all_specs_in_goal_coding <- function(x, coding_from, coding_to) {
  x <- sort(unique(stats::na.omit(x))) # sort for convenience, not a
  # technical requirement

  codes_from <- fe_species_get_coding_table(coding_from) |>
    dplyr::filter(.data$species_id %in% vctrs::vec_data(x)) |>
    dplyr::rename(species_id_from = "species_id")

  codes_to <- fe_species_get_coding_table(coding_to) |>
    dplyr::rename(species_id_to = "species_id")


  goal_not_exist_tab <- codes_from |>
    dplyr::left_join(codes_to, by = c("genus", "species_no"))

  rslt <- !is.na(goal_not_exist_tab$species_id_to)

  rqmt_ok <- all(rslt)
  err_message <- paste(
    "Original code(s)",
    paste(unique(goal_not_exist_tab[!rslt, ]$species_id_from), collapse = ", "),
    "has/have no (complete) match in goal species coding."
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message,
    no_match_table = goal_not_exist_tab |>
      dplyr::filter(is.na(.data$species_id_to)) |>
      dplyr::select("species_id_from", "genus", "species_no")
  )
}




#' Check if an Intended Species Coding Cast is Unambiguous
#'
#' @param x Vector of species codes (\code{character})
#'
#' @param coding_from Original species coding name (character)
#'
#' @param coding_to Goal coding name (character)
#'
#' @return A list with three elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements of x), 3)
#'   \code{ambiguity_table}, a data.frame showing the number of goal
#'   species_id's each original species_id is corresponding to
#'
#' @noRd
#'
#' @examples
#' ForestElementsR:::spec_id_cast_unambiguous(
#'   as.character(c(1, 2, 8)), "tum_wwk_short", "ger_nfi_2012"
#' )
#'
spec_id_cast_unambiguous <- function(x, coding_from, coding_to) {
  x <- sort(unique(stats::na.omit(x))) # sort for convenience, not a
  # technical requirement

  codes_from <- fe_species_get_coding_table(coding_from) |>
    dplyr::filter(.data$species_id %in% vctrs::vec_data(x)) |>
    dplyr::rename(species_id_from = "species_id")

  codes_to <- fe_species_get_coding_table(coding_to) |>
    dplyr::rename(species_id_to = "species_id")

  ambi_tab <- codes_from |>
    dplyr::left_join(codes_to, by = c("genus", "species_no")) |>
    dplyr::group_by(.data$species_id_from) |>
    dplyr::summarise(
      n_species_id_to = length(unique(.data$species_id_to))
    )

  rslt <- ambi_tab$n_species_id_to == 1

  rqmt_ok <- all(rslt)
  err_message <- paste(
    "Ambiguous cast attempt. Original code(s)",
    paste(ambi_tab[!rslt, ]$species_id_from, collapse = ", "),
    "correspond(s) to",
    paste(ambi_tab[!rslt, ]$n_species_id_to, collapse = ", "),
    "goal code(s)."
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message,
    ambiguity_table = ambi_tab
  )
}




#' Check if an Intended Species Coding Cast Loses Information
#'
#' Note that a cast where only one species_id from the original coding
#' translates in a goal coding which represents a group of species is NOT
#' considered lossy (i.e. backward ambiguous), because of the 1:1 match in the
#' constellation of the specific cast.
#'
#' @param x Vector of species codes (\code{character}) or an object of an
#'   **fe_species** class
#'
#' @param coding_from Original species coding name (character)
#'
#' @param coding_to Goal coding name (character)
#'
#' @return A list with three elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements of x), 3)
#'   \code{ambiguity_table}, a data.frame showing the number of original
#'   species_id's each goal species_id is corresponding to
#'
#' @noRd
#'
#' @examples
#' ForestElementsR:::spec_id_cast_loss_free(
#'   x = as.character(c(6, 3, 5, 1, 5, 5, 1, 1, 8)),
#'   coding_from = "tum_wwk_short",
#'   coding_to = "bavrn_state"
#' )
#'
spec_id_cast_loss_free <- function(x, coding_from, coding_to) {
  x <- sort(unique(stats::na.omit(x))) # sort for convenience, not a
  # technical requirement

  codes_from <- fe_species_get_coding_table(coding_from) |>
    dplyr::filter(.data$species_id %in% vctrs::vec_data(x)) |>
    dplyr::rename(species_id_from = "species_id")

  codes_to <- fe_species_get_coding_table(coding_to) |>
    dplyr::rename(species_id_to = "species_id")

  ambi_tab <- codes_from |>
    dplyr::left_join(codes_to, by = c("genus", "species_no")) |>
    dplyr::group_by(.data$species_id_to) |>
    dplyr::summarise(
      n_species_id_from = length(unique(.data$species_id_from))
    )

  rslt <- ambi_tab$n_species_id_from == 1

  rqmt_ok <- all(rslt)
  err_message <- paste(
    "Cast loses information. Goal code(s)",
    paste(ambi_tab[!rslt, ]$species_id_to, collapse = ", "),
    "correspond to",
    paste(ambi_tab[!rslt, ]$n_species_id_from, collapse = ", "),
    "original code(s)."
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message,
    ambiguity_table = ambi_tab
  )
}



#' Validate an Intended Cast from One **fe_species** to Another
#'
#' Terminates with an error if the intended cast is forward ambiguous, and
#' issues a warning, if the cast would come with a loss of information (i.e.
#' backward ambiguous)
#'
#' Note that a cast where only one species_id from the original coding
#' translates in a goal coding which represents a group of species is NOT
#' considered backward ambiguous, because of the 1:1 match in the constellation
#' of the specific cast.
#'
#' @param x Vector of species codes (\code{character}) or an object of an
#'   **fe_species** class
#'
#' @param coding_from Original species coding name (character). While this
#'   information could be drawn from \code{x} when it is an **fe_species**
#'   object, we want it to be provided explicitly
#'
#' @param coding_to Goal coding name (character)
#'
#' @return If the cast is forward unambigous, the original \code{x} will be
#'   returned invisibly. Otherwise the function terminates with an error. A
#'   warning is issued in case the cast would lose information (i.e. cast
#'   several species which are distinct in the original coding into the same
#'   group code in the goal coding).
#'
#' @noRd
#'
#' @examples
#' spec_ids <- fe_species_bavrn_state(c("10", "20", "20"))
#' ForestElementsR:::spec_id_cast_validate(
#'   spec_ids, "bavrn_state", "tum_wwk_short"
#' )
#'
#' # Would generate an error
#' # spec_ids <- fe_species_ger_nfi_2012(c("19", "20", "24"))
#' # ForestElementsR:::spec_id_cast_validate(
#' #   spec_ids, "ger_nfi_2012", "tum_wwk_long"
#' # )
#'
spec_id_cast_validate <- function(x, coding_from, coding_to) {
  rslt <- spec_id_cast_all_specs_in_goal_coding(x, coding_from, coding_to)
  if (!rslt$rqmt_ok) {
    stop(rslt$err_message, call. = FALSE)
  }

  rslt <- spec_id_cast_unambiguous(x, coding_from, coding_to)
  if (!rslt$rqmt_ok) {
    stop(rslt$err_message, call. = FALSE)
  }

  rslt <- spec_id_cast_loss_free(x, coding_from, coding_to)
  if (!rslt$rqmt_ok) {
    warning(rslt$err_message, call. = FALSE)
  }

  invisible(x)
}



#' Perform an Species ID Cast
#'
#' Performs an actual species id cast between two **fe_species** codings, but
#' terminates with an error if the cast were forward ambiguous. A warning is
#' issued (but the cast is performed) if it comes with a loss of information.
#'
#' @param x x Vector of species codes (\code{character}) or an object of an
#'   **fe_species** class
#'
#' @param coding_from Original species coding name (character). While this
#'   information could be drawn from \code{x} when it is an **fe_species**
#'   object, we want it to be provided explicitly
#'
#' @param coding_to Goal coding name (character)
#'
#' @return Always a \code{character} vector in the same order as \code{x}
#'   containing the cast species id's. It is up to the calling function to
#'   transform this into an **fe_species** object.
#'
#' @noRd
#'
#' @examples
#' spec_ids <- fe_species_bavrn_state(c("10", "20", "20"))
#' ForestElementsR:::spec_id_cast_do_it(
#'   spec_ids, "bavrn_state", "tum_wwk_short"
#' )
#'
#' # generates an error
#' # spec_ids <- fe_species_ger_nfi_2012(c("19", "20", "24"))
#' # ForestElementsR:::spec_id_cast_do_it(
#' #   spec_ids, "ger_nfi_2012", "tum_wwk_long"
#' # )
#'
spec_id_cast_do_it <- function(x, coding_from, coding_to) {
  x <- vctrs::vec_data(x)

  # Check cast. If the casting is ambigous, we will stop here with an error ...
  spec_id_cast_validate(x, coding_from, coding_to)

  # ... so after this point casting is safe
  codes_from <- fe_species_get_coding_table(coding_from) |>
    dplyr::filter(.data$species_id %in% vctrs::vec_data(x)) |>
    dplyr::rename(species_id_from = "species_id")

  codes_to <- fe_species_get_coding_table(coding_to) |>
    dplyr::rename(species_id_to = "species_id")

  transltn_tab <- codes_from |>
    dplyr::left_join(codes_to, by = c("genus", "species_no"))

  # make translation vector
  trans_vec <- transltn_tab |> purrr::pluck("species_id_to")
  names(trans_vec) <- transltn_tab |> purrr::pluck("species_id_from")

  # translate
  rslt <- trans_vec[x]
  names(rslt) <- NULL

  rslt # always character, must be made fe_species in the caller
}
