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




#' Static Stand Sum and Mean Values for an fe_stand Object
#'
#' Calculate ha-wise static stand sum and mean values for a fe_stand object. The
#' term 'static' means that no growth and increment variables are calculated,
#' only descriptive variables for each point in time.
#'
#' Default setting for the dominant heights and diameters is the method by
#' Weise, i.e. quadratic mean diameter and height for the 20% biggest trees.
#' Alternatively, the d100, h100 method by Assmann can be selected. This should
#' be, however, done with care, because d100 and h100 are only well defined
#' in monospecific stands. Note, that this function does not take into account
#' species shares in mixed stands when calculating d100 and h100.
#'
#'
#' @param x An \code{\link{fe_stand}} object
#'
#'
#' @param tree_filter A \code{data-masking} expression that applies to the
#'   data.frame \code{x$trees}. It must return a logical value, and is defined
#'   in terms of the variables in \code{x$trees}. In this function, it is used
#'   internally in order to define the cohort of trees which is to be evaluated
#'   by this function (within a call to \code{dplyr::filter()}). While many
#'   meaningful filterings are conceivable, distinctions between total stand,
#'   removal stand, and remaining stand are the most probable applications.
#'   Defaults to \code{TRUE}, i.e. all trees are included. See examples.
#'
#' @param hd_dom_method Method for calculationg the dominant diameter and
#'   dominant height. The default choice is "Weise", using the functions
#'   \code{\link{d_dom_weise}}, and \code{\link{h_dom_weise}}. Alternatively,
#'   the option "Assmann" uses the functions \code{\link{d_100}}, and
#'   \code{\link{h_100}}.
#'
#'
#' @return A data frame (tibble) with the ha-related static sum values stem
#'   number, basal area, volume, quadratic mean diameter, dominant diameter,
#'   quadratic mean height, dominant height. If no tree in \code{x$trees} passes
#'   \code{tree_filter}, as defined above, an empty data frame is returned. In
#'   case an object of class \code{fe_ccircle_spatial_notrees} (which is a
#'   special child of \code{\link{fe_stand}}) is provided as input \code{x}, the
#'   function also returns an empty data frame.
#'
#'
#' @export
#'
#' @examples
#' # Evaluation for all trees
#' mm_forest_1_fe_stand_spatial |> stand_sums_static()
#'
#' # Exclude removal trees
#' mm_forest_1_fe_stand_spatial |> stand_sums_static(!removal)
#'
#' # Exclude removal trees and include only trees with dbh > 30 cm
#' mm_forest_1_fe_stand_spatial |> stand_sums_static(!removal & dbh_cm > 30)
#'
#' # Exclude removal trees, use Assmann's d100 h100 for dominant height
#' # and diameter
#' mm_forest_1_fe_stand_spatial |>
#'   stand_sums_static(!removal, hd_dom_method = "Assmann")
#'
#' # Include all trees, use Assmann's d100 h100 for dominant height
#' # and diameter
#' mm_forest_1_fe_stand_spatial |>
#'   stand_sums_static(hd_dom_method = "Assmann")
#'
stand_sums_static <- function(x,
                              tree_filter = TRUE,
                              hd_dom_method = c("Weise", "Assmann")) {
  # Stop if first argument has wrong type
  if (!is_fe_stand(x)) {
    stop("Input object is not an fe_stand", call. = FALSE)
  }

  # Special case fe_ccircle_spatial_notrees
  # Return an empty data frame
  if (is_fe_ccircle_spatial_notrees(x)) {
    message("Returning empty data frame for class fe_ccircle_spatial_notrees")
    return(data.frame())
  }

  # Make a copy of x. This might look overblown, but future versions of this
  # function might require to use the unaltered x, while a modified copy is
  # used for calculations
  work_dat <- x

  # remove any trees with n_rep == 0 (can occur in fe_stand_spatial)
  work_dat$trees <- work_dat$trees |>
    dplyr::filter(.data$n_rep_ha > 0)


  # Filter tree data as specified by the user
  work_dat$trees <- work_dat$trees |>
    dplyr::filter({{ tree_filter }})


  # Calculate (ha-scaled) tree volumes here, because this is much faster than
  # doing inside the call to summarise below. Trees without height get an NA
  # volume
  suppressWarnings(
    work_dat$trees <- work_dat$trees |>
      dplyr::mutate(.v_temp = ifelse(is.na(.data$height_m),
        NA,
        .data$n_rep_ha *
          v_gri(
            .data$species_id,
            .data$dbh_cm,
            .data$height_m
          )
      ))
  )

  # The choice for how to calculate dominant heights and diameters must be
  # evaluated before following block
  hd_dom_method <- match.arg(hd_dom_method)

  # Make summary
  nn <- nrow(work_dat$trees)

  work_dat$trees |>
    dplyr::group_by(.data$time_yr, .data$species_id) |>
    dplyr::summarise(
      stem_number_ha = sum(.data$n_rep_ha),
      basal_area_m2_ha = sum((.data$dbh_cm / 100)^2 * pi / 4 * .data$n_rep_ha),
      d_q_cm = d_q(.data$dbh_cm, .data$n_rep_ha),
      d_dom_cm = if (nn == 0) {
        NA
      } else {
        switch(hd_dom_method,
          Weise   = d_dom_weise(.data$dbh_cm, .data$n_rep_ha),
          Assmann = d_100(.data$dbh_cm, .data$n_rep_ha)
        )
      },
      h_q_m = if (any(is.na(.data$height_m))) {
        NA
      } else {
        h_q(.data$height_m, .data$dbh_cm, .data$n_rep_ha)
      },
      h_dom_m = if ((nn == 0) | any(is.na(.data$height_m))) {
        NA
      } else {
        switch(hd_dom_method,
          Weise = h_dom_weise(
            .data$height_m, .data$dbh_cm, .data$n_rep_ha
          ),
          Assmann = h_100(
            .data$height_m, .data$dbh_cm, .data$n_rep_ha
          )
        )
      },
      v_m3_ha = sum(.data$.v_temp)
    )
}
