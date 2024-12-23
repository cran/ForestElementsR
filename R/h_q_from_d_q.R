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




#' Estimate Quadratic Mean Height from Quadratic Mean Diameter
#'
#' Fallback function to be used in the unfortunate case when a mean stand
#' (cohort) height value is required but nothing useful is contained in the
#' available data. The function has originally been developed for and used in
#' the forest growth simulator SILVA
#' \insertCite{pretzsch_single_2002}{ForestElementsR}. The version here is
#' slightly simplified for the species Norway spruce and soft deciduous woods
#' (codes 1 and 9 in the species coding \code{\link{fe_species_tum_wwk_short}}),
#' because the original version requires non-standard information which is not
#' available outside the simulator.
#'
#' A typical application of this function would be to calculate the quadratic
#' mean diameter, \code{\link{d_q}} of the tree cohort of interest. If height
#' information is totally lacking, a reasonable fallback value of the
#' corresponding quadratic mean height, h_q, can be obtained from this function.
#' Both values could then be used as the entry point of a standard height curve
#' system (like \code{\link{h_standard_gnfi3}} and \code{\link{h_standard_bv}})
#' in order to get plausible height estimates for the single trees in the
#' cohort. Be aware that, though the height estimates obtained from this
#' function are plausible, they should be interpreted with care, because the
#' variation of mean stand height at a given mean diameter is very high in
#' reality.
#'
#' @param species_id vector of species codes in any format that can be converted
#'   into the tum_wwk_short species coding (see examples)
#'
#' @param d_q_cm vector of quadratic mean stand (cohort) diameters in cm for
#'   which a corresponding mean height estimate is required
#'
#' @return vector of estimated quadratic mean heights corresponding to the given
#'   values of \code{d_q_cm}
#'
#' @family stand heights
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'  # Species codes handed to h_q_from_d_q will be attempted to convert
#'  # into tum_wwk_short
#'  h_q_from_d_q(5, 25.4) # Apply to a common beech stand width d_q = 25.4 cm
#'  h_q_from_d_q(fe_species_ger_nfi_2012(100), 25.4)
#'
#'  # Works also vectorized, i.e. when vectors of d_q values and species codes
#'  # are given
#'  species <- fe_species_tum_wwk_short(c(1, 3, 5))
#'  d_qmean <- c(23.2, 47.2, 12.7)
#'  h_q_from_d_q(species, d_qmean)
#'
#' # Typical application: From diameter values to single tree height estimates
#' # when no height information is available
#' # - single tree diameters in a stand/cohort
#' dbh <- c(10.2, 43.3, 37.5, 28.8, 12.4, 19.2, 25.4, 27.3, 32.0)
#' # - quadratic mean diameter
#' d_qmean <- d_q(dbh)
#' species <- fe_species_tum_wwk_short(3) # we assume it's a Scots pine stand
#' # - quadrativ mean height fallback value
#' h_qest <- h_q_from_d_q(species, d_qmean)
#' # - single tree height estimates with the German National Inventory height
#' #   curve system
#' h_standard_gnfi3(species, dbh, d_qmean, h_qest)
#'
h_q_from_d_q <- function(species_id, d_q_cm) {
  UseMethod("h_q_from_d_q")
}


#' @export
h_q_from_d_q.default <- function(species_id, d_q_cm) {

  species_id <- vctrs::vec_data(as_fe_species_tum_wwk_short(species_id))
  h_q_from_d_q_core(species_id, d_q_cm)
}


#' @export
h_q_from_d_q.fe_species_tum_wwk_short <- function(species_id, d_q_cm) {

  species_id <- vctrs::vec_data(species_id)
  h_q_from_d_q_core(species_id, d_q_cm)
}


#' Core Function for Estimating Quadratic Mean Height from Quadratic Mean
#' Diameter
#'
#' See documentation of \code{\link{h_q_from_d_q}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user.
#'
#' @param species_id when handed to this core function, species_id must be
#'   character and correspond to the tum_wwk_short species coding
#'   \code{\link{fe_species_tum_wwk_short}}
#'
#' @param d_q_cm see \code{\link{h_q_from_d_q}}
#'
#' @return see \code{\link{h_q_from_d_q}}
#'
#' @keywords internal
#'
h_q_from_d_q_core <- function(species_id, d_q_cm) {

  # get input data into a workable format
  # if the input vectors cannot be coerced into a tibble, this will raise an
  # error. Additional checks are not required here.
  work_dat <- tibble::tibble(
    species_id = species_id,
    d_q_cm     = d_q_cm
  )

  # link data to the function parameters stored in param_h_q_from_d_q
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(param_h_q_from_d_q) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # Calculate the h_q values
  work_dat |> purrr::pmap_dbl(
    .f = function(species_id, d_q_cm, a0, a1, a2) {
      1.3 + a0 * (1 - exp(-a1 * d_q_cm) ^ a2)
    }
  )
}







