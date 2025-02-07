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




#' Constructor for the **fe_tally_list** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_stand}} for creating an
#' object of that class.
#'
#' @param x An appropriate \code{list} object
#'
#' @param ... Additional arguments required for enabling subclasses of
#'   \code{fe_tally_list}
#'
#' @param class A Character string required for enabling subclasses of
#'   \code{fe_tally_list}
#'
#' @return An object of class \code{fe_tally_list}
#'
#' @keywords internal
#'
#'
new_fe_tally_list <- function(x = list(), ..., class = character()) {

  stopifnot(is.list(x))
  parent_classes <- class(new_fe_tally_list(x))
  structure(
    x,
    ...,
    class = c(class, "fe_tally_list", parent_classes)
  )
}



#' Check if an Object is an **fe_tally_list**
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_tally_list}
#'   class
#'
#' @keywords internal
#'
#'
is_fe_tally_list <- function(x) {
  inherits(x, "fe_tally_list")
}


#' Validate an **fe_tally_list** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{new_fe_tally_list}}.
#' Regular users, please construct \code{fe_tally_list} objects with
#' \code{\link{fe_tally_list}}.
#'
#' @param x An object that is expected to be a correct \code{fe_tally_list}
#'   object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the \code{fe_tally_list}
#'   object specifications. In case of such violations, the function will
#'   terminate with an error.
#'
#' @keywords internal
#'
validate_fe_tally_list <- function(x) {
  # obviously similar but not equal to the validation of an fe_stand object

  # Check for class attribute
  stopifnot(is_fe_tally_list(x))


  # Check for presence of the required list elements
  required_names <- c(
   "stand_id", "area_ha", "tree_counts", "class_definition"
  )
  check_rslt <- has_required_names(x, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "element(s)", check_rslt$err_message, "missing in fe_tally_list object"
      ),
      call. = FALSE
    )
  }


  # Check for correct types or classes of the list elements
  # outline is checked separately, because it can by also NULL
  required_types_or_classes <- tibble::tribble(
    ~name, ~required,
    "area_ha", "numeric",
    "tree_counts", "data.frame",
    "class_definition", "data.frame",
  )

  check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("element(s)", check_rslt$err_message),
      call. = FALSE
    )
  }



  ## Check for required column types (empty columns (if allowed) must have the
  ## correct NA type), species_id is not included here, will be checked below
  required_types <- tibble::tribble(
    ~name,                    ~required,
    "stand_id",             "character",
    "size_class",           "character",
    "Nr",                     "numeric",
    "N",                      "numeric"
  )
  check_rslt <- has_required_types_or_classes(x$tree_counts, required_types)


  # Check species_id
  # - Coding supported?
  spcs_coding <- fe_species_get_coding(x$tree_counts$species_id)
  if (length(spcs_coding) == 0) spcs_coding <- ""

  if (!(spcs_coding %in% ForestElementsR::species_codings$species_coding)) {
    stop(
      paste(
        "data.frame `tree_counts`:",
        "unsupported species coding of column `species_id`"
      ),
      call. = FALSE
    )
  }


  ## Check for required column types (empty columns (if allowed) must have the
  ## correct NA type), species_id is not included here, will be checked below
  required_types <- tibble::tribble(
    ~name,                    ~required,
    "size_class",           "character",
    "class_type",           "character",
    "class_min",              "numeric",
    "class_max",              "numeric"
  )
  check_rslt <- has_required_types_or_classes(x$class_definition, required_types)



  # Return the object independently from validation result
  x
}



#' User Friendly Construction of an **fe_tally_list** Object from a List of
#' Data Frames
#'
#' \code{fe_tally_list()} provides a user-friendly interface for the
#' constructor \code{\link{new_fe_tally_list}}. While the constructor does
#' not prevent users from creating malformed \code{fe_tally_list} objects,
#' \code{fe_tally_list} does everything to achieve a well-defined object
#' mostly based on an initial list of data.frames that might be, e.g. drawn out
#' of a user's own data base.
#'
#'
#' The input object \code{x} to \code{fe_tally_list} must be a list that
#' comprises two data frame(s):
#' \itemize{ \item a data frame containing the tree counts per class.
#' This data frame must contain a a minimum set of columns (Nr, size_class, N,
#' species.
#'
#' \item a data frame that contains information about the class definition, }
#'
#' @param x named list of two or three data frames to be coerced into the goal
#'   object. One data frame must contain the tree counts data
#'
#' @param stand_id arbitrary id of the stand (\code{character}, default:
#'   "my_tally_list")
#'
#' @param tree_count_name name of the data frame in \code{x} that contains the
#'    tree data counting (default: "tree_count")
#'
#' @param class_def_name name of the column in the trees data frame which
#' provides the definition of the counting classes (default: "class_definition")
#'
#' @param area_ha_name name of the column in the trees data frame which provides
#'   each tree's representation number per ha.
#'
#' @param size_class_col description
#'
#' @param tree_nr_col description
#'
#' @param species_id_col description
#'
#' @param count_col description
#'
#' @param class_type_col description
#'
#' @param class_min_col description
#'
#' @param class_max_col description
#'
#'
#' @return If the user input allows to construct a well-defined
#'   \code{fe_tally_list} object, this object will be returned. If not, the
#'   function will terminate with an error.
#'
#' @keywords internal
#'
fe_tally_list <- function(x,
                          stand_id = "my_tally_list",
                          tree_count_name = "tree_count",
                          class_def_name = "class_definition",
                          area_ha_name = "area_ha",
                          size_class_col,
                          tree_nr_col,
                          species_id_col,
                          count_col,
                          class_type_col,
                          class_min_col,
                          class_max_col){


  # Make the core data.frame trees of an fe_stand_spatial object

  tree_counts <- x[[tree_count_name]]

  tree_counts <-  tree_counts |>
    dplyr::select(tidyselect::all_of(c(.data$size_class_col,
                                       .data$tree_nr_col,
                                       .data$species_id_col,
                                       .data$count_col))) |>
    dplyr::rename(size_class = .data$size_class_col,
                  Nr =         .data$tree_nr_col,
                  species_id =    .data$species_id_col,
                  N =          .data$count_col)


  class_definition <- x[[class_def_name]]

  class_definition <-  class_definition |>
    dplyr::select(tidyselect::all_of(c(.data$size_class_col,
                                       .data$class_type_col,
                                       .data$class_min_col,
                                       .data$class_max_col))) |>
    dplyr::rename(size_class = .data$size_class_col,
                  class_type = .data$class_type_col,
                  class_min =  .data$class_min_col,
                  class_max =  .data$class_max_col)

  area_ha <- x[[area_ha]]

  # Combine all ingredients into a list
  # No checks required here, because this will be covered by the validator
  # which is called below after the constructor
  fe_tally_list_candidate <- list(
    stand_id    = trimws(stand_id, which = "both"),
    tree_counts = tree_counts,
    class_definition = class_definition,
    area_ha = area_ha
  )

  # Most important: call the constructor
  fe_tally_list_candidate <- new_fe_tally_list(
    fe_tally_list_candidate
  )

  # Validate the object
  fe_tally_list_candidate <- validate_fe_tally_list(fe_tally_list_candidate)

  # validated candidate
  fe_tally_list_candidate
}




























