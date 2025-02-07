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




#' Reduce a Given standing Volume Over Bark to Harvested Volume Under Bark
#'
#' Many tree volume functions (like \code{\link{v_gri}}) calculate wood volumes
#' defined as standing and over bark. Practictioners often prefer to work with
#' volumes where the harvest losses and the bark volume have been substracted.
#' Given an over bark standing volume, this function uses species specific
#' reduction factors in order to obtain harvested volume under bark. The
#' reduction factors are taken from
#' \insertCite{hilfstafeln_bavaria_1990}{ForestElementsR}; they relate to the
#' species coding \code{fe_species_tum_wwk_short} or, alternatively
#' \code{fe_species_bavrn_state_short}.
#'
#' @param species_id Vector of species id's. Ideally, these species_id's are
#'   provided as a \code{\link{fe_species_tum_wwk_short}} or a
#'   \code{\link{fe_species_bavrn_state_short}} object. If they are provided as
#'   another \code{fe_species} object, \code{v_red_harvest_ubark} will make an
#'   attempt to convert them into \code{\link{fe_species_tum_wwk_short}}. The
#'   exception is the coding \code{\link{fe_species_bavrn_state}} which will be
#'   converted into \code{\link{fe_species_bavrn_state_short}}. If all
#'   conversion attempts fail, the function will terminate with an error. The
#'   species id's can also be provided as numeric values (\code{double} or
#'   \code{integer}) or \code{character}. These will be internally converted to
#'   \code{\link{fe_species_tum_wwk_short}}. If this fails (i.e. the user
#'   provided species codes are not defined in the *tum_wwk_short* coding), an
#'   error is thrown and the function terminates.
#'
#' @param v_orig_m3 Vector of wood volumes (m³) defined as standing over bark.
#'   If \code{species_id} and \code{v_orig_m3} do not have the same length, an
#'   attempt is made to recyle them according to the tibble rules.
#'
#' @return A vector of the reduced volumes defined as harveest
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   # Take all species groups of tum_wwk_short and a standing volume of 1 m³
#'   # over bark
#'   species_id <- fe_species_tum_wwk_short(1:10)
#'   v_red_harvest_ubark(species_id, 1)
#'
v_red_harvest_ubark <- function(species_id, v_orig_m3) {
  UseMethod("v_red_harvest_ubark")
}



# v_red_harvest_ubark.default applies to all cases, where species_id is not
# given as n tum_wwk_short object. It makes a conversion attempt and calculates
# the the volumes if the conversion is successful.

#' @export
v_red_harvest_ubark.default <- function(species_id, v_orig_m3) {
  species_id <- vctrs::vec_data(as_fe_species_tum_wwk_short(species_id))
  v_red_harvest_ubark_core(
    species_id, v_orig_m3, param_volred_harv_ubark_tum_wwk_short
  )
}



# If species_id is already given as a tum_wwk_short object, no conversion
# is required

#' @export
v_red_harvest_ubark.fe_species_tum_wwk_short <- function(species_id,
                                                         v_orig_m3) {
  species_id <- vctrs::vec_data(species_id)
  v_red_harvest_ubark_core(
    species_id, v_orig_m3, param_volred_harv_ubark_tum_wwk_short
  )
}


# If species_id is already given as a bavrn_state_short object, no conversion
# is required

#' @export
v_red_harvest_ubark.fe_species_bavrn_state_short <- function(species_id,
                                                             v_orig_m3) {

  species_id <- vctrs::vec_data(species_id)
  v_red_harvest_ubark_core(
    species_id, v_orig_m3, param_volred_harv_ubark_bavrn_state_short
  )
}


# If species id's are given as bavrn_state, they will be converted into
# bavrn_state_short

#' @export
v_red_harvest_ubark.fe_species_bavrn_state <- function(species_id,
                                                       v_orig_m3) {

  species_id <- vctrs::vec_data(as_fe_species_bavrn_state_short(species_id))
  v_red_harvest_ubark_core(
    species_id, v_orig_m3, param_volred_harv_ubark_bavrn_state_short
  )
}




#' Core Function For the Volume Reduction With v_red_harvest_ubark
#'
#' See documentation of \code{\link{v_red_harvest_ubark}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user.
#'
#' The function \code{v_red_harvest_ubark} is fully vectorized, inputs are
#' recyled according to the tibble rules
#'
#' @param species_id Vector of species ids (see
#'   \code{\link{v_red_harvest_ubark}}), but for this core function to work,
#'   \code{species_id} must be provided as \code{character}. This will and
#'   should be not checked inside this function, because if used as intended,
#'   that has happened before calling it.
#'
#' @param v_orig_m3 Vector of wood volumes (m³) defined as standing over bark
#'   (see documentation of \code{\link{v_red_harvest_ubark}}.)
#'
#' @param params A data frame comprising species (group) wise reduction factors.
#'   The calling function has to take care that \code{v_red_harvest_ubark_core}
#'   is called with a \code{params} that fits to the coding used in
#'   \code{species_id}.
#'
#' @return A vector of wood volumes defined as harvested under bark (see
#'   \code{\link{v_red_harvest_ubark}})
#'
#' @keywords internal
#'
v_red_harvest_ubark_core <- function(species_id, v_orig_m3, params) {

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    v_orig_m3  = v_orig_m3
  )

  # link data to the function parameters stored in v_gri_param
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(params) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # Do the actual calculation and hand back the vector of reduced volumes
  work_dat |>
    dplyr::mutate(vol_harv_ubark = .data$v_orig_m3 *
                    (1 - .data$harv_loss) *
                    (1 - .data$bark_red)) |>
    purrr::pluck("vol_harv_ubark")
}








