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



#' Assert That a Species Code Vector Contains Only Tree Species
#'
#' Used by the \code{fe_stand} object validators: non-tree codes (e.g. a shrub
#' category, see \code{\link{fe_species_non_tree_codes}}) must not enter an
#' \code{fe_stand} object or any of its children, because the package's methods
#' work on trees only. Terminates with an error naming the offending codes if
#' any non-tree species is present; \code{NA} codes are ignored. Codings without
#' non-tree codes pass trivially.
#'
#' @param species_id An object of one of the \code{fe_species} classes
#'
#' @param where Short context string for the error message
#'
#' @return Invisibly \code{species_id}; called for the side effect of erroring
#'
#' @keywords internal
#' @noRd
#'
.assert_tree_species <- function(species_id, where = "column `species_id`") {
  is_t <- fe_species_is_tree(species_id)
  bad  <- !is_t & !is.na(is_t)
  if (any(bad)) {
    codes <- unique(vctrs::vec_data(species_id)[bad])
    stop(sprintf(
      paste0("%s: non-tree species code(s) %s are not allowed in an fe_stand ",
             "object (methods work on trees only); remove these records first."),
      where, paste(codes, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(species_id)
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




#' Reduce a Coding Table to the Finest Code per Master Species
#'
#' For a hierarchical coding a single master species may appear under several
#' codes (e.g. a single-species "leaf" code and one or more group codes that
#' contain it). For casting *into* a coding we always want the finest available
#' representation of a species. This helper keeps, for each \code{(genus,
#' species_no)}, the row belonging to the finest code.
#'
#' "Finest" is determined by the column \code{level} if present (smaller =
#' finer), otherwise by the species-set size (the number of master species
#' sharing a \code{species_id}; smaller = finer). Under the laminarity
#' invariant the codes containing a given master species form a chain, so the
#' finest one is unique. For partition codings (each master species in exactly
#' one code) this function is a no-op, including row order.
#'
#' @param coding_table A coding table as returned by
#'   \code{\link{fe_species_get_coding_table}}
#'
#' @return The coding table reduced to one row per \code{(genus, species_no)}
#'
#' @keywords internal
#' @noRd
#'
.finest_per_species <- function(coding_table) {
  if ("level" %in% names(coding_table)) {
    rnk <- coding_table[["level"]]
  } else {
    set_size <- table(coding_table[["species_id"]])
    rnk <- as.integer(set_size[as.character(coding_table[["species_id"]])])
  }
  key <- paste(coding_table[["genus"]], coding_table[["species_no"]],
               sep = "\r")
  # Finest (= smallest) rank available per master species, in original order
  min_rnk <- stats::ave(rnk, key, FUN = min)
  keep    <- rnk == min_rnk
  ct      <- coding_table[keep, , drop = FALSE]
  # Should ties at the finest rank occur, keep the first in original order
  ct[!duplicated(key[keep]), , drop = FALSE]
}



#' Validate that a Coding Table Forms a Laminar Hierarchy
#'
#' A hierarchical coding may map a master species to several codes (a fine
#' "leaf" code and coarser group codes containing it), but the species-sets of
#' any two codes must be either disjoint or nested (laminar). This validator
#' enforces that invariant and a few related ones. It is meant to be called
#' when a coding table is built (e.g. from a CSV).
#'
#' Checks performed:
#' \itemize{
#'   \item No two distinct codes cover an identical species-set.
#'   \item The species-sets of any two codes are disjoint or nested (no partial
#'     overlap).
#'   \item If a \code{level} column is present: each code has a single level,
#'     and a code nested inside another has a strictly smaller level.
#' }
#' Rows without a master link (\code{genus}/\code{species_no} \code{NA}, e.g.
#' non-tree codes) are ignored, as they carry no species-set.
#'
#' @param coding_table A coding table as returned by
#'   \code{\link{fe_species_get_coding_table}}
#'
#' @param coding_name Optional name of the coding, used in error messages
#'
#' @return Invisibly \code{coding_table}; called for the side effect of
#'   terminating with an error on any violation.
#'
#' @keywords internal
#' @noRd
#'
.validate_coding_laminar <- function(coding_table, coding_name = NULL) {
  cn <- if (is.null(coding_name)) "" else paste0(" '", coding_name, "'")

  # Only rows that link to a master species carry a species-set
  link <- !is.na(coding_table$genus) & !is.na(coding_table$species_no)
  ct   <- coding_table[link, , drop = FALSE]

  key  <- paste(ct$genus, ct$species_no, sep = "\r")
  sets <- lapply(split(key, ct$species_id), unique)
  ids  <- names(sets)

  has_level <- "level" %in% names(coding_table)
  if (has_level) {
    lvl_per_id <- vapply(
      split(coding_table$level, coding_table$species_id),
      function(z) {
        u <- unique(z)
        if (length(u) != 1L) NA_real_ else as.numeric(u)
      },
      numeric(1)
    )
    if (anyNA(lvl_per_id)) {
      bad <- names(lvl_per_id)[is.na(lvl_per_id)]
      stop(sprintf("Coding%s: code(s) %s have non-unique level values.",
                   cn, paste(bad, collapse = ", ")), call. = FALSE)
    }
  }

  for (i in seq_along(sets)) {
    for (j in seq_len(i - 1L)) {
      a <- sets[[i]]
      b <- sets[[j]]
      inter <- length(intersect(a, b))
      if (inter == 0L) next # disjoint, fine
      la <- length(a)
      lb <- length(b)
      if (inter < min(la, lb)) {
        stop(sprintf(
          "Coding%s is not laminar: codes '%s' and '%s' partially overlap.",
          cn, ids[i], ids[j]
        ), call. = FALSE)
      }
      if (la == lb) {
        stop(sprintf(
          "Coding%s: codes '%s' and '%s' cover an identical species set.",
          cn, ids[i], ids[j]
        ), call. = FALSE)
      }
      if (has_level) {
        smaller <- if (la < lb) ids[i] else ids[j]
        larger  <- if (la < lb) ids[j] else ids[i]
        if (!(lvl_per_id[[smaller]] < lvl_per_id[[larger]])) {
          stop(sprintf(
            paste0("Coding%s: level of nested code '%s' (%s) is not smaller ",
                   "than that of '%s' (%s)."),
            cn, smaller, lvl_per_id[[smaller]], larger, lvl_per_id[[larger]]
          ), call. = FALSE)
        }
      }
    }
  }

  invisible(coding_table)
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

  codes_to <- .finest_per_species(fe_species_get_coding_table(coding_to)) |>
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

  codes_to <- .finest_per_species(fe_species_get_coding_table(coding_to)) |>
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

  codes_to <- .finest_per_species(fe_species_get_coding_table(coding_to)) |>
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
#' issues a message, if the cast would come with a loss of information (i.e.
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
#'   message is issued in case the cast would lose information (i.e. cast
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
    # Information loss when casting fine codes into a coarser group is the
    # normal, intended outcome of an aggregation, so it is signalled as a
    # message (not a warning). Forward ambiguity above stays an error.
    message(rslt$err_message)
  }

  invisible(x)
}



#' Declarative Registry of Species Coding Cast Overrides
#'
#' Some source group codes straddle several groups in a goal coding and thus
#' have no single matching target node (genuine forward ambiguity, e.g.
#' \code{ger_nfi_2012} code "290", which maps mostly to \code{tum_wwk_short}
#' "8" but contains one species belonging to "9"). For such cases this registry
#' declares the deliberately chosen target code. \code{\link{spec_id_cast_do_it}}
#' consults it: a matching source code is resolved to the registered target
#' (with a message) instead of raising an ambiguity error. The resolution is
#' lossy by nature and is the maintainer's deliberate choice. Keyed by
#' \code{(coding_from, coding_to, species_id_from)}.
#'
#' The registry is the package data object \code{\link{species_cast_overrides}},
#' built from the editable CSV \code{data-raw/codings/cast_overrides.csv} via
#' \code{\link{cast_overrides_from_csv}} (CSV-driven, like the codings).
#'
#' @return A tibble with columns \code{coding_from}, \code{coding_to},
#'   \code{species_id_from}, \code{species_id_to}
#'
#' @noRd
#'
spec_id_cast_overrides <- function() {
  ForestElementsR::species_cast_overrides
}



#' Perform an Species ID Cast
#'
#' Performs an actual species id cast between two **fe_species** codings, but
#' terminates with an error if the cast were forward ambiguous. A message is
#' issued (but the cast is performed) if it comes with a loss of information.
#'
#' Forward-ambiguous source codes for which a deliberate target is declared in
#' \code{\link{spec_id_cast_overrides}} are resolved to that target with a
#' message instead of raising an error.
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

  # Declarative overrides for otherwise forward-ambiguous casts (replaces the
  # former hard-wiring, e.g. ger_nfi_2012 "290" -> tum_wwk_short "8"). Only
  # entries whose source code actually occurs in x are relevant.
  ov <- spec_id_cast_overrides()
  ov <- ov[ov$coding_from == coding_from &
             ov$coding_to == coding_to &
             ov$species_id_from %in% x, , drop = FALSE]
  ov_pos <- x %in% ov$species_id_from

  if (nrow(ov) > 0) {
    message(
      "Applied cast override(s) ", coding_from, " -> ", coding_to, ": ",
      paste(ov$species_id_from, "->", ov$species_id_to, collapse = ", ")
    )
  }

  # Non-tree source codes (e.g. a shrub category) have no species equivalent in
  # any species coding -> they become NA (with a message). Like overrides they
  # are kept out of the regular cast, but they are NOT patched back, so they
  # stay NA. (In normal use non-tree records are already rejected at fe_stand
  # object construction; this is the graceful fallback for bare casts.)
  nt_codes <- fe_species_non_tree_codes(coding_from)
  nt_pos   <- x %in% nt_codes
  if (any(nt_pos)) {
    message(
      "Non-tree code(s) ", paste(unique(x[nt_pos]), collapse = ", "),
      " have no equivalent in coding '", coding_to, "' and become NA."
    )
  }

  # Overridden and non-tree codes are excluded from the regular (validated)
  # cast (set to NA, which also keeps them out of the ambiguity check);
  # overridden codes are patched back afterwards, non-tree codes stay NA.
  x_reg <- x
  x_reg[ov_pos | nt_pos] <- NA

  # Check cast. If the (remaining) casting is ambiguous, we stop here with an
  # error ...
  spec_id_cast_validate(x_reg, coding_from, coding_to)

  # ... so after this point casting is safe
  codes_from <- fe_species_get_coding_table(coding_from) |>
    dplyr::filter(.data$species_id %in% x_reg) |>
    dplyr::rename(species_id_from = "species_id")

  codes_to <- .finest_per_species(fe_species_get_coding_table(coding_to)) |>
    dplyr::rename(species_id_to = "species_id")

  transltn_tab <- codes_from |>
    dplyr::left_join(codes_to, by = c("genus", "species_no"))

  # make translation vector
  trans_vec <- transltn_tab |> purrr::pluck("species_id_to")
  names(trans_vec) <- transltn_tab |> purrr::pluck("species_id_from")

  # translate
  rslt <- trans_vec[x_reg]
  names(rslt) <- NULL

  # patch in the deliberately chosen override targets
  if (nrow(ov) > 0) {
    ov_map <- stats::setNames(ov$species_id_to, ov$species_id_from)
    rslt[ov_pos] <- unname(ov_map[x[ov_pos]])
  }

  rslt # always character, must be made fe_species in the caller
}
