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




#' Generate an Overview of the Surveys of an fe_stand Object
#'
#' The tree data frame of an \code{\link{fe_stand}} object is evaluated in order
#' to get survey and species specific meta information about these data.
#'
#' This function provides meta information that is useful for evaluations that
#' can be complex on the detail level, e.g. increment calculations from
#' subsequent surveys. In such contexts, the column \code{n_species_occurence}
#' of the output data frame can be of special interest. If a species is present
#' for a number of consecutive surveys, all these surveys get the same integer
#' number in this column. If the same species vanishes, but occurs again later,
#' the next block of surveys gets the subsequent number, and so on. So, all
#' consecutive blocks of a species' occurrence are numbered as 1, 2, 3, etc.
#' At surveys where the species is not present \code{n_species_occurrence} has
#' the value 1.
#'
#' @param x An object of class \code{\link{fe_stand}}
#'
#' @param tree_filter  A \code{data-masking} expression that applies to the
#'   data.frame \code{x$trees}. It must return a logical value, and is defined
#'   in terms of the variables in \code{x$trees}. In this function, it is used
#'   internally in order to define the cohort of trees which is to be evaluated
#'   by this function (within a call to \code{dplyr::filter()}). For this
#'   function, \code{tree_filter} should almost \emph{never} be something else
#'   than \code{TRUE} (default)
#'
#' @return A data frame (tibble) that gives an overview of the surveys
#'   represented in the input object \code{x}. It is basically an evaluation of
#'   the data frame \code{x$trees}. It provides information about how many trees
#'   were present in each survey, how many dbh and heights were measured, and
#'   if the dbh and height measurements cover all trees, For dbh this must be
#'   always true, because this is a requirement for a valid
#'   \code{\link{fe_stand}} object. In addition, we are informed whether a
#'   species that has been documented in the object is represented in a specific
#'   survey or not. As a basis for advanced evaluations, species occurences are
#'   numbered in the columen \code{n_species_occurrence} (see Details). In case
#'   an object of class \code{fe_ccircle_spatial_notrees} (which is a special
#'   child of \code{\link{fe_stand}}) is provided as input \code{x}, the
#'   function returns an empty data frame.
#'
#' @export
#'
#' @examples
#'   # Example data: Mixed mountain forest plot with several surveys
#'   mm_forest_1_fe_stand_spatial |> survey_overview()
#'
survey_overview <- function(x, tree_filter = TRUE) {

  # Stop if first argument has wrong type
  if (!is_fe_stand(x)) {
    stop("Input object is not an fe_stand")
  }

  # Special case fe_ccircle_spatial_notrees
  # Return an empty data frame
  if (is_fe_ccircle_spatial_notrees(x)) {
    message("Returning empty data frame for class fe_ccircle_spatial_notrees")
    return(data.frame())
  }


  work_dat <- x

  # Apply tree_filter
  work_dat$trees <- work_dat$trees |>
    dplyr::filter({{ tree_filter }})

  # Make a base data frame that contains all possible combinations of survey
  # times and species
  base_frame <- work_dat$trees |>
    tidyr::expand(.data$species_id, .data$time_yr)

  # Make an evaluation data frame of what is actually there
  eval_frame <- work_dat$trees |>
    dplyr::group_by(.data$species_id, .data$time_yr) |>
    dplyr::summarise(
      n_trees = dplyr::n(),                 # should always be the same as
      n_dbh   = sum(!is.na(.data$dbh_cm)),  # n_dbh; required for fe_stand class
      n_h     = sum(!is.na(.data$height_m)),
      dbh_complete = .data$n_dbh == .data$n_trees,      # big problem when FALSE
      h_complete   = .data$n_h   == .data$n_trees
   )

  # Merge both frames and calculate metadata
  overview_frame <- base_frame |>
    dplyr::left_join(eval_frame) |>
    dplyr::mutate(
      species_there = !is.na(.data$n_trees),
      n_trees       = ifelse(is.na(.data$n_trees), 0, .data$n_trees)
    )

  # For counting occurrences and reoccurrences of species we split
  # overview_frame by species
  overview_frame <- split(
    overview_frame, f = unclass(overview_frame$species_id)
  )

  overview_frame |>
    purrr::map(
      .f = function(dat) {
        dat <- dat |> dplyr::arrange(.data$time_yr) # for safety's sake
        rle_rslt       <- rle(dat$species_there)
        occurr_numbers <- cumsum(rle_rslt$values) * rle_rslt$values
        dat$n_species_occurrence <- inverse.rle(
          list(lengths = rle_rslt$lengths, values = occurr_numbers)
        )
        dat
      }
    ) |>
    dplyr::bind_rows()
}
