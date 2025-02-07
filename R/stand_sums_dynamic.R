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




#' stand_sums_dynamic
#'
#' Calculate periodic annual volume and basal area increments for
#' \code{\link{fe_stand}} objects with repeated surveys
#'
#' For the sake of robustness, \code{stand_sums_dynamic} does not perform a
#' plausibility check on single tree level before calculating increments.
#' Internally, the function \code{\link{stand_sums_static}} is called separately
#' for the remaining and the removal stand. Both are required for calculating
#' meaningful increments. Basal area increments are always calculated, because
#' valid \code{\link{fe_stand}} objects always contain the required information
#' (i.e. all trees' dbh); volume increments are calculated if also height values
#' (either measured or estimated) are available for all trees.
#'
#' @param x An \code{\link{fe_stand}} object
#'
#' @param tree_filter A \code{data-masking} expression that applies to the
#'   data.frame \code{x$trees}. It must return a logical value, and is defined
#'   in terms of the variables in \code{x$trees}. In this function, it is used
#'   internally in order to define the cohort of trees which is to be evaluated
#'   by this function (within a call to \code{dplyr::filter()}). In order to
#'   obtain meaningful increments, you should be very careful when changing
#'   the default value (\code{TRUE}) of \code{tree_filter}.
#'
#' @return A data frame (tibble) that contains the periodic annual basal area
#'   and volume growth (the latter if enough information is available,
#'   see Details) per ha for each species and each period. The year where the
#'   increment entry is, means the end point of the period of interest.
#'   Therefore, the first increment value will always be NA. If the input
#'   \code{\link{fe_stand}} object \code{x} does only comprise one survey, the
#'   increment values will be NA, because it takes at least two subsequent
#'   surveys to calculate meaningful increments. In case an object of class
#'   \code{fe_ccircle_spatial_notrees} (which is a special child of
#'   \code{\link{fe_stand}}) is provided as input \code{x}, the function returns
#'   an empty data frame.
#'
#' @export
#'
#' @examples
#'   oo <- options(fe_spec_lang = "eng") # Display species names in English
#'
#'   # Mixed mountain forest with several surveys
#'   stand_inc <- stand_sums_dynamic(mm_forest_1_fe_stand_spatial)
#'   stand_inc
#'
#'   # Combine to species overarching increments. Zero in the first year results
#'   # as there cannot be increments available at the first survey
#'   stand_inc |>
#'     dplyr::group_by(time_yr) |>
#'       dplyr::summarise(
#'         iba_m2_ha_yr = sum(iba_m2_ha_yr, na.rm = TRUE),
#'         iv_m3_ha_yr  = sum(iv_m3_ha_yr,  na.rm = TRUE)
#'       )
#'
#'   # When there is only one single survey, all increments must be NA
#'   stand_sums_dynamic(spruce_beech_1_fe_stand)
#'
#'   options(oo) # Set options to previous values
#'
stand_sums_dynamic <- function(x, tree_filter = TRUE) {

  # Stop if first argument has wrong type
  if (!is_fe_stand(x)) {
    stop("Input object is not an fe_stand", call. = TRUE)
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

  # Make survey overview on the filtered object, contains (among others) all
  # possible combinations of the survey times and species codes, and the
  # in formation if a species is there at a given survey or not.
  survey_ov <- survey_overview(work_dat) |>
    dplyr::select(
      .data$species_id, .data$time_yr, .data$h_complete, .data$species_there,
      .data$n_species_occurrence
    )

  # Make static stand sums (on the filtered work dat) for the remaining and
  # the removal stand
  static_remain <- stand_sums_static(work_dat, tree_filter = !.data$removal)
  static_remove <- stand_sums_static(work_dat, tree_filter =  .data$removal)

  static_remain <- static_remain |>
    dplyr::select(
      .data$time_yr, .data$species_id, .data$basal_area_m2_ha, .data$v_m3_ha,
    ) |>
    dplyr::rename(rem_basal_area_m2_ha = .data$basal_area_m2_ha,
                  rem_v_m3_ha          = .data$v_m3_ha)

  static_remove <- static_remove |>
    dplyr::select(
      .data$time_yr, .data$species_id, .data$basal_area_m2_ha, .data$v_m3_ha,
    ) |>
    dplyr::rename(rmv_basal_area_m2_ha = .data$basal_area_m2_ha,
                  rmv_v_m3_ha          = .data$v_m3_ha)

  # Join everything together with the relevant columns of survey_of the the left
  static_comb <- survey_ov |>
    dplyr::select(
      .data$time_yr, .data$species_id, .data$species_there,
      .data$n_species_occurrence, .data$h_complete
    ) |>
    dplyr::left_join(static_remain) |>
    dplyr::left_join(static_remove) |>
    dplyr::filter(.data$species_there)

  # Replace NA basal areas with zero (no check required, because we have
  # already filtered on species_there
  static_comb <- static_comb |>
    dplyr::mutate(
      rem_basal_area_m2_ha = ifelse(!is.na(.data$rem_basal_area_m2_ha),
                                    .data$rem_basal_area_m2_ha,
                                    0),
      rmv_basal_area_m2_ha = ifelse(!is.na(.data$rmv_basal_area_m2_ha),
                                    .data$rmv_basal_area_m2_ha,
                                    0)
    )

  # Replace NA volumes with zero if h_complete, because that means volumes
  # have been calculated and NA entries are only due to either no removal or no
  # remaining stand (species_there is guaranteed by the filter above)
  static_comb <- static_comb |>
    dplyr::mutate(
      rem_v_m3_ha = ifelse(.data$h_complete & is.na(.data$rem_v_m3_ha),
                           0,
                           .data$rem_v_m3_ha),
      rmv_v_m3_ha = ifelse(.data$h_complete & is.na(.data$rmv_v_m3_ha),
                           0,
                           .data$rmv_v_m3_ha)
    )

  # Remove columns we do not require anymore
  static_comb <- static_comb |>
    dplyr::select(-.data$species_there, -.data$h_complete)

  # We split static_comb by species. No split by layers, because users who
  # require layer-specific evaluations should do that by setting tree_filter
  # accordingly.
  # What we do, however, we split by n_species_occurrence. This allows us to
  # correctly handly a species that disappeared and reappeared later again.
  static_comb <- split(
    static_comb,
    f = list(unclass(static_comb$species_id), static_comb$n_species_occurrence)
  )

  # Apply stand_level_increment to each list element, always calculating
  # the basal area increment. Volume increment is calculated, whenever the
  # required data are complete.
  static_comb <- static_comb |>
    purrr::map(
      .f = function(dat) {
        # Sort for being on the safe side
        dat <- dat |> dplyr::arrange(.data$time_yr)
        # Calculate basal area increment (always possible)
        dat <- dat |>
          dplyr::mutate(
            iba_m2_ha_yr = stand_level_increment(
              .data$time_yr,
              .data$rem_basal_area_m2_ha,
              .data$rmv_basal_area_m2_ha)
          )
        # If volume information is complete, calculate volume increment
        if (all(!is.na(dat$rem_v_m3_ha)) & all(!is.na(dat$rmv_v_m3_ha))) {
          dat <- dat |>
            dplyr::mutate(
              iv_m3_ha_yr = stand_level_increment(
                .data$time_yr,
                .data$rem_v_m3_ha,
                .data$rmv_v_m3_ha
              )
            )
        } else {
          dat <- dat |>
            dplyr::mutate(
              iv_m3_ha_yr = NA
            )
        }
        # return input with additional increment colums
        dat
      }
    )

  # Make static_comb great again (list -> one tibble), take away the columns
  # that are not required anymore, give back the outcome
  static_comb |>
    dplyr::bind_rows() |>
    dplyr::select(
      -.data$rem_basal_area_m2_ha, -.data$rem_v_m3_ha,
      -.data$rmv_basal_area_m2_ha, -.data$rmv_v_m3_ha,
      -.data$n_species_occurrence
    )
}



