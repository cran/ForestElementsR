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




#' species_shares
#'
#' Calculate tree species shares for a \code{\link{fe_stand}} object. Different
#' methods and scopes are available.
#'
#' The calculation uses the \code{trees} data frame of the input
#' \code{\link{fe_stand}} object. The small tree cohort is not taken into
#' account. Three different methods are available to choose from (parameter
#' \code{method}: \describe{ \item{\bold{Basal area shares, weighted with wood
#' density (method "ba_wd"): }}{ The species shares are based on basal areas
#' which are weighted with the species specific wood densities raised to the
#' power of 2/3. The exponent of 2/3 takes into account that wood density is a
#' three-dimensional quantity, while basal area is two-dimensional. This is the
#' default method. It works, however, only with species codings that can be
#' converted into the \code{\link{fe_species_tum_wwk_short}} coding (default),
#' or with \code{\link{fe_species_bavrn_state_short}}. The latter is used if
#' this species coding is directly provided or if the species coding is
#' \code{\link{fe_species_bavrn_state}}. The reason for that restriction is that
#' wood densities are currently only provided for the two species codings
#' \code{\link{fe_species_tum_wwk_short}}, and
#' \code{\link{fe_species_bavrn_state_short}}. The resulting shares, however,
#' will always relate to the original coding.} \item{\bold{Unweighted basal area
#' shares (method "ba"):}}{ Species shares are calculated as shares of the
#' unweighted basal areas. } \item{\bold{Stem number shares (method "n"):}}{
#' Species shares are calculated as stem number shares, i.e. tree size does not
#' matter for that calculation. } }
#'
#'
#' @param x An \code{\link{fe_stand}} object
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
#' @param method Character string defining the calculation method to be applied.
#'   Must be one of "ba_wd" (default), "ba", and "n" (see Details).
#'
#' @param include_ingrowth If \code{TRUE} (default), newly ingrown trees will be
#'   included in the calculation.
#'
#' @return A data frame (tibble) with the three columns \code{species_id},
#'   \code{time_yr}, and \code{species_share}. If no tree passes the
#'   user-defined \code{tree_filter}, the tibble will have no lines.
#'
#' @export
#'
#' @examples
#' species_shares(selection_forest_1_fe_stand) # default method ("ba_wd")
#' species_shares(selection_forest_1_fe_stand, method = "ba")
#' species_shares(selection_forest_1_fe_stand, method = "n")
#'
#' # Same stand, different cohorts
#' mm_forest_1_fe_stand_spatial |> species_shares() # all trees
#' mm_forest_1_fe_stand_spatial |> species_shares(!removal) # remaining only
#' mm_forest_1_fe_stand_spatial |> species_shares(removal) # removal only
#'
species_shares <- function(x,
                           tree_filter = TRUE,
                           method = c("ba_wd", "ba", "n"),
                           include_ingrowth = TRUE) {
  # Stop if first argument has wrong type
  if (!is_fe_stand(x)) {
    stop("Input object is not an fe_stand", call. = FALSE)
  }

  work_dat <- x$trees

  # remove any trees with n_rep == 0 (can occur in fe_stand_spatial)
  work_dat <- work_dat |>
    dplyr::filter(.data$n_rep_ha > 0)


  # Filter tree data as specified by the user
  work_dat <- work_dat |>
    dplyr::filter({{ tree_filter }})


  method <- match.arg(method)

  # Wood density weighted calculation makes only sense if species coding is
  # translateable into tum_wwk_short (default) or bavrn_state_short
  if (method == "ba_wd") {
    suppressWarnings(
      tryCatch(

        if (is_fe_species_bavrn_state(work_dat$species_id) |
            is_fe_species_bavrn_state_short(work_dat$species_id)) {
          work_dat$wd_spec <- as_fe_species_bavrn_state_short(
            work_dat$species_id
          )
        }
        else {
          work_dat$wd_spec <- work_dat$tws_spec <- as_fe_species_tum_wwk_short(
            work_dat$species_id
          )
        },

        error = function(e) {
          stop(
            paste(
              "Species coding not compatible with wood density weighted",
              "share calculation"
            ),
            call. = FALSE
          )
        }
      ) # tryCatch
    ) # suppressWarnings

    if (is_fe_species_bavrn_state_short(work_dat$wd_spec))
      wood_dens <- wood_density_bavrn_state_short
    else
      wood_dens <- wood_density_tum_wwk_short

    work_dat <- work_dat |>
      dplyr::left_join(
        wood_dens,
        by = c("wd_spec" = "species_id")
      ) |>
      dplyr::mutate(
        share_var =
          .data$dbh_cm^2 * .data$wood_density^(2 / 3) * .data$n_rep_ha
      )
  }

  if (method == "ba") {
    work_dat <- work_dat |>
      dplyr::mutate(share_var = .data$dbh_cm^2 * .data$n_rep_ha)
  }

  if (method == "n") {
    work_dat <- work_dat |>
      dplyr::mutate(share_var = .data$n_rep_ha)
  }


  work_smry_spec_time <- work_dat |>
    dplyr::group_by(.data$species_id, .data$time_yr) |>
    dplyr::summarise(share_var = sum(.data$share_var))

  work_smry_time <- work_dat |>
    dplyr::group_by(.data$time_yr) |>
    dplyr::summarise(share_var_total = sum(.data$share_var))

  work_smry_spec_time |>
    dplyr::left_join(work_smry_time) |>
    dplyr::mutate(species_share = .data$share_var / .data$share_var_total) |>
    dplyr::select(.data$species_id, .data$time_yr, .data$species_share) |>
    dplyr::ungroup()
}
