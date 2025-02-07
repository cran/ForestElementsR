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




#' Estimate the growth of stand, or inventory point based on yield tables
#'
#' @param object an object or child of class \code{\link{fe_stand}}
#' containing the stand or inventory data
#'
#' @param yield_table_1 character string with the name of the yield table to be
#' used for Norway spruce, default: yield table for Norway spruce by Wiedemann
#' 1936/42
#'
#' @param yield_table_2 character string with the name of the yield table to be
#' used for silver fir, default: yield table for silver fir by Hausser 1956
#'
#' @param yield_table_3 character string with the name of the yield table to be
#' used for Scots pine, default: yield table for pine by Gehrhardt 1921
#'
#' @param yield_table_4 character string with the name of the yield table to be
#' used for European larch, default: yield table for European larch by Schober
#' 1946
#'
#' @param yield_table_5 character string with the name of the yield table to be
#' used for European beech, default: yield table for beech by Wiedemann 1931
#'
#' @param yield_table_6 character string with the name of the yield table to be
#' used for sessile/pedunculate oak, default: yield table for oak by Juettner
#' 1955
#'
#' @param yield_table_7 character string with the name of the yield table to be
#' used for Douglas fir, default: yield table for Douglas fir by Schober 1956
#'
#' @param yield_table_8 character string with the name of the yield table to be
#' used for other hardwoods, default: yield table for ash by Wimmenauer 1919/29
#'
#' @param yield_table_9 character string with the name of the yield table to be
#' used for soft deciduous wood species, default: yield table for black alder by
#' Mitscherlich 1945
#'
#' @param yield_table_10 character string with the name of the yield table to be
#' used for other conifers, default: yield table for Scots pine by Gehrhardt
#' 1921
#'
#' @param variable_name a character string specifying the entry variable for the
#' yield table, standard is "h_q_m"
#'
#' @return a tibble with growth data for the main stand and species and the
#' total growth corrected with NFI species shares
#'
#' @keywords internal
#'
#' @noRd
#'
#'
#' @examples
#'
#' # Monospecific stand of European beech
#'
#' growth_est_ytable(european_beech_1_fe_stand,
#'   yield_table_5 = "fe_ytable_beech_wiedemann_moderate_1931",
#'   variable_name = "h_q_m"
#' )
#'
#' # Mixed stand of Norway spruce and European beech
#' growth_est_ytable(spruce_beech_1_fe_stand,
#'   yield_table_1 = "fe_ytable_spruce_wiedemann_moderate_1936_42",
#'   yield_table_5 = "fe_ytable_beech_wiedemann_moderate_1931",
#'   variable_name = "h_q_m"
#' )
#'
growth_est_ytable <- function(
    object,
    # spruce
    yield_table_1 = "fe_ytable_spruce_wiedemann_moderate_1936_42",
    # silver fir
    yield_table_2 = "fe_ytable_silver_fir_hausser_moderate_1956",
    # pine
    yield_table_3 = "fe_ytable_pine_gehrhardt_moderate_1921",
    # larch
    yield_table_4 = "fe_ytable_larch_schober_moderate_1946",
    # beech
    yield_table_5 = "fe_ytable_beech_wiedemann_moderate_1931",
    # oak
    yield_table_6 = "fe_ytable_oak_juettner_moderate_1955",
    # douglas fir
    # here we just know the one NOT to be used :)
    yield_table_7 = "fe_ytable_douglas_schober_moderate_1956",
    # hard broadleaves
    yield_table_8 = "fe_ytable_ash_wimmenauer_1919_29",
    # soft broadleaves
    yield_table_9 = "fe_ytable_blackalder_mitscherlich_heavy_1945",
    # other conifers
    yield_table_10 = "fe_ytable_pine_gehrhardt_moderate_1921",
    variable_name = "h_q_m"
) {

  # check if the object is of class fe_stand
  stopifnot(is_fe_stand(object))

  # check if the yield_table is of class fe_yield_table
  stopifnot(is_fe_yield_table(get(yield_table_1, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_2, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_3, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_4, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_5, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_6, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_7, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_8, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_9, envir = parent.frame())))
  stopifnot(is_fe_yield_table(get(yield_table_10, envir = parent.frame())))

  # extract the trees from the object
  trees <- object[["trees"]]

  # check if age_yr has values in trees if not give an error
  if (is.na(mean(trees$age_yr))) {
    stop(
      "The age of all trees must be given",
      call. = FALSE
    )
  }

  # if the column for layer is included (layer_key == 1) filter the main stand
  # otherwise take all trees
  if ("layer_key" %in% names(trees)) {
    trees <- trees |> dplyr::filter(.data$layer_key == 1)
  }


  trees <- trees |>
    dplyr::mutate(
      species_id = as_fe_species_tum_wwk_short(.data$species_id),
      yield_table = dplyr::case_when(
        .data$species_id == as_fe_species_tum_wwk_short(1)  ~ yield_table_1,
        .data$species_id == as_fe_species_tum_wwk_short(2)  ~ yield_table_2,
        .data$species_id == as_fe_species_tum_wwk_short(3)  ~ yield_table_3,
        .data$species_id == as_fe_species_tum_wwk_short(4)  ~ yield_table_4,
        .data$species_id == as_fe_species_tum_wwk_short(5)  ~ yield_table_5,
        .data$species_id == as_fe_species_tum_wwk_short(6)  ~ yield_table_6,
        .data$species_id == as_fe_species_tum_wwk_short(7)  ~ yield_table_7,
        .data$species_id == as_fe_species_tum_wwk_short(8)  ~ yield_table_8,
        .data$species_id == as_fe_species_tum_wwk_short(9)  ~ yield_table_9,
        .data$species_id == as_fe_species_tum_wwk_short(10) ~ yield_table_10
      )
    )

  # group the main stand by species and age and estimate needed parameters
  stand_shares <- trees |>
    dplyr::mutate(
      g_m2 = (.data$dbh_cm / 100)^2 * pi / 4 * .data$n_rep_ha,
      standing_area_m2_ha = standing_area_gnfi3(
        .data$species_id,
        .data$dbh_cm
      ) * .data$n_rep_ha, # standing area per tree
      total_stand_area_m2_ha = sum(.data$standing_area_m2_ha)
    ) |> # total standing area in the plot
    dplyr::group_by(.data$species_id, .data$age_yr) |>
    dplyr::summarize(
      h_q_m = ForestElementsR::h_q(
        .data$height_m,
        .data$dbh_cm,
        .data$n_rep_ha,
        na_h.rm = TRUE
      ),
      g_m2 = sum(.data$g_m2, na.rm = TRUE),
      total_g_m2 = sum(.data$g_m2),
      shares_g = .data$g_m2 / .data$total_g_m2,
      shares_nfi = sum(.data$standing_area_m2_ha) /
        unique(.data$total_stand_area_m2_ha),
      # corrected basal are for the monospecific stand
      g_m2_corr = .data$g_m2 / .data$shares_nfi,
      yield_table = unique(.data$yield_table)
    ) |>
    dplyr::group_by(.data$species_id, .data$age_yr) |>
    # calculate site index per species and age groups
    dplyr::mutate(
      site_index = ForestElementsR::site_index(
        age = .data$age_yr,
        size = .data$h_q_m,
        ytable = get(.data$yield_table),
        si_variable = variable_name
      ),
      stocking_level = ForestElementsR::stocking_level(
        ba = .data$g_m2_corr,
        age = .data$age_yr,
        si = .data$site_index,
        ytable = get(.data$yield_table, envir = parent.frame())
      )
    )
  # dplyr::ungroup()

  # extract pai from the yield table for each species and age groups
  pai_species_age <- stand_shares |>
    dplyr::mutate(
      pai = ytable_lookup(
        si = .data$site_index,
        age = .data$age_yr,
        variable = "pai_m3_ha_yr",
        ytable = get(.data$yield_table, envir = parent.frame())
      ),
      # correct growth with stocking level
      pai_m3_ha_yr = .data$pai * min(1, .data$stocking_level)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(total_pai_m3_ha_yr = stats::weighted.mean(
      .data$pai_m3_ha_yr,
      .data$shares_nfi)
    ) |>
    dplyr::select(
      .data$species_id,
      .data$age_yr,
      .data$h_q_m,
      .data$site_index,
      .data$stocking_level,
      .data$shares_nfi,
      .data$pai_m3_ha_yr,
      .data$total_pai_m3_ha_yr,
      .data$yield_table
    )

  pai_species_age
}
