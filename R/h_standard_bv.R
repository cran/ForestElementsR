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




#' Calculate Tree Heights with the Bavarian Standard Height Curve System
#'
#' Implementation of the standard height curve system used by the Bavarian State
#' Forest Service \insertCite{kennel_r_beech_bavaria_1972}{ForestElementsR}. The
#' structure of the approach was developed by R. Kennel together with a
#' parameterisation for European beech (Fagus sylvatica). Later, anonymous
#' scientists have extended the parameters for all species (groups) covered by
#' the *tum_wwk_short* species coding. The standard height curve system
#' allows to estimate a tree's height when its dbh is given together with the
#' quadratic mean diameter, the corresponding quadratic mean height, and the age
#' of the stand it belongs to.
#'
#' In order to provide maximum flexibility in applying the function
#' h_standard_bv, the stand values (age, mean height, mean diameter) can be
#' provided with each tree diameter individually. This allows estimating heights
#' for trees from different stands at the same time. In the same way, the
#' provided species codes are not required to be the same for each tree.
#'
#' @param species_id Vector of species id's following the *tum_wwk_short* or the
#'   *bavrn_state_short* species coding. If another coding is provided, an
#'   attempt will be made to convert it into the "nearest" of the two codings
#'   mentioned above. The default is a conversion attempt to *tum_wwk_short*.
#'   The species id's can also be provided as numeric values
#'   (\code{double} or \code{integer}) or \code{character}. These will be
#'   interpreted as and converted to \code{fe_species_tum_wwk_short}.
#'   If all conversion attempts fail, the function will terminate with an
#'   error.
#'
#' @param dbh_cm Vector of tree dbh values in cm (dbh = stem diameter at breast
#'   height, i.e. 1.3 m)
#'
#' @param age_yr Vector of stand age values in years (will be recycled following
#'   the rules for tibbles)
#'
#' @param d_q_cm Vector of quadratic mean stand diameters (will be recycled
#'   following the rules for tibbles)
#'
#' @param h_q_m Vector of quadratic mean stand heights (will be recycled
#'   following the rules for tibbles)
#'
#' @return A vector of the estimated heights
#'
#' @family standard height curve systems
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   species_id <- fe_species_tum_wwk_short(rep(3, 7))  # Seven Scots pines
#'   dbh <- c(10.1, 27.4, 31.4, 35.5, 39.8, 45.2, 47.2) # and their diameters
#'   # Estimate the heights of these trees assuming they are from a 100 year old
#'   # stand with a mean diameter of 35.5 cm, and a corresponding mean height
#'   # of 28 m.
#'   h_standard_bv(species_id, dbh, age_yr = 100, d_q_cm = 35.5, h_q_m = 28.0)
#'
#'   # Compare with sister function h_standard_gnfi3 which does not require
#'   # stand age
#'   h_standard_gnfi3(species_id, dbh, d_q_cm = 35.5, h_q_m = 28.0)
#'
h_standard_bv <- function(species_id, dbh_cm, age_yr, d_q_cm, h_q_m) {
  UseMethod("h_standard_bv")
}


# This default function should cover most cases. An attempt will be made to
# convert species_id into into tum_wwk_short, which is meaningful for all cases
# except when species_id is already given as tum_wwk_short

#' @export
h_standard_bv.default <- function(species_id = species_id,
                                  dbh_cm     = dbh_cm,
                                  age_yr     = age_yr,
                                  d_q_cm     = d_q_cm,
                                  h_q_m      = h_q_m) {

  species_id <- as_fe_species_tum_wwk_short(species_id) |>
    vctrs::vec_data()

  h_standard_bv_core(species_id, dbh_cm, age_yr, d_q_cm, h_q_m,
                     params = h_standard_bv_tum_wwk_short)
}



# In case species_id is given as tum_wwk_short, no conversion is required

#' @export
h_standard_bv.fe_species_tum_wwk_short <- function(species_id,
                                                   dbh_cm,
                                                   age_yr,
                                                   d_q_cm,
                                                   h_q_m) {
  species_id <- vctrs::vec_data(species_id)
  h_standard_bv_core(species_id, dbh_cm, age_yr, d_q_cm, h_q_m,
                     params = h_standard_bv_tum_wwk_short)
}


# In case species_id is given as bavrn_state_short, no conversion is required

#' @export
h_standard_bv.fe_species_bavrn_state_short <- function(species_id,
                                                       dbh_cm,
                                                       age_yr,
                                                       d_q_cm,
                                                       h_q_m) {
  species_id <- vctrs::vec_data(species_id)
  h_standard_bv_core(species_id, dbh_cm, age_yr, d_q_cm, h_q_m,
                     params = h_standard_bv_bavrn_state_short)
}


# For tum_wwk_long, conversion into tum_wwk_short is necessary

#' @export
h_standard_bv.fe_species_tum_wwk_long <- function(species_id,
                                                   dbh_cm,
                                                   age_yr,
                                                   d_q_cm,
                                                   h_q_m) {

  species_id <- as_fe_species_tum_wwk_short(species_id) |>
    vctrs::vec_data()

  h_standard_bv_core(species_id, dbh_cm, age_yr, d_q_cm, h_q_m,
                     params = h_standard_bv_tum_wwk_short)
}


# For bavrn_state, conversion into bavrn_state_short is necessary

#' @export
h_standard_bv.fe_species_bavrn_state <- function(species_id,
                                                 dbh_cm,
                                                 age_yr,
                                                 d_q_cm,
                                                 h_q_m) {

  species_id <- as_fe_species_bavrn_state_short(species_id) |>
    vctrs::vec_data()

  h_standard_bv_core(species_id, dbh_cm, age_yr, d_q_cm, h_q_m,
                     params = h_standard_bv_bavrn_state_short)
}



#' Core Function for calculating Individual Tree Heights with the Bavarian
#' Standard Height Curve System
#'
#' See documentation of \code{\link{h_standard_bv}} for most information, this
#' function is the workhorse behind, and should never be called directly by a
#' user.
#'
#' The function \code{h_standard_bv} is fully vectorized, inputs are recyled
#' according to the tibble rules
#'
#' @param species_id Vector of species ids (see \code{\link{h_standard_bv}}),
#'   but for this core function to work, \code{species_id} must be provided as
#'   \code{character}. This will and should be not checked inside this function,
#'   because if used as intended, that has happened before calling it.
#'
#' @param dbh_cm Diameter at breast height vector (see documentation of
#'   \code{\link{h_standard_bv}})
#'
#' @param age_yr Stand age vector (see documentation of
#'   \code{\link{h_standard_bv}})
#'
#' @param d_q_cm Stand mean diameter vector (see documentation of
#'   \code{\link{h_standard_bv}})
#'
#' @param h_q_m Stand mean height vector (see documentation of
#'   \code{\link{h_standard_bv}})
#'
#' @param params A data frame that defines the function parameters corresponding
#'   to the species coding represented by \code{species_id}
#'
#' @return A vector of the estimated heights
#'
#' @keywords internal
#'
h_standard_bv_core <- function(species_id,
                               dbh_cm,
                               age_yr,
                               d_q_cm,
                               h_q_m,
                               params) {

  # Check if any of the input vectors is longer than dbh_cm and stop with an
  # error if so. Otherwise, dbh_cm would be recycled in some cases, which is
  # undesired. All other possible problems will be adequately handled by
  # tibble::tibble below
  length_in <- c(
    length(species_id), length(age_yr), length(d_q_cm), length(h_q_m)
  )
  if(any(length_in > length(dbh_cm))) {
    stop("No input vector must be longer than dbh_cm.")
  }

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    dbh_cm     = dbh_cm,
    age_yr     = age_yr,
    d_q_cm     = d_q_cm,
    h_q_m      = h_q_m
  )

  # link data to the function parameters stored in param_h_q_from_d_q
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(params) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # Calculate the heights
  work_dat |> purrr::pmap_dbl(
    .f = function(species_id, dbh_cm, age_yr, d_q_cm, h_q_m, a0, a1, a2) {
      dwp    <- exp(a0 + a1 * log(age_yr) + a2 * age_yr) + 0.4
      hlp_1  <- (h_q_m - 1.3) ^ (1/3)
      hlp_2  <- 1 + dwp / d_q_cm
      a      <- 1 / (hlp_1 * hlp_2) # a and b are Petterson height curve
      b      <- a * dwp             # parameters

      1.3 + (a + b / dbh_cm) ^ -3   # Petterson height estimate
    }
  )
}


