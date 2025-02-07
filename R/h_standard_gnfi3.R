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




#' Calculate Tree Heights with the Bavarian Standard Height Curve System of the
#' 3rd German National Forest Inventory (2012)
#'
#' Implementation of the standard height curve developed during the the 3nd
#' German National Forest Inventory
#' \insertCite{bwi3_methods_2017}{ForestElementsR}. Structurally, this is a
#' height curve system after Sloboda
#' \insertCite{sloboda_et_al_1993}{ForestElementsR} which allows to estimate a
#' tree's height when its species, diameter, and the quadratic mean diameter
#' and height of (the species in) the stand is given.
#'
#' Originally, the height curve system was parameterized for species and species
#' groups corresponding to the national forest inventory's species coding
#' (\code{\link{fe_species_ger_nfi_2012}}).  We have attributed in addition these
#' the original parameters also to the species codings
#' \code{\link{fe_species_tum_wwk_short}}, and
#' \code{\link{fe_species_bavrn_state_short}}. When called with a given species
#' coding, the function will try to use the "nearest" of these three
#' alternatives. Fallback option is the attempt to use
#' \code{\link{fe_species_tum_wwk_short}}.
#'
#' In order to provide maximum flexibility in applying the function
#' h_standard_gnfi3, the stand values (mean height, mean diameter) can be
#' provided with each tree diameter individually. This allows estimating heights
#' for trees from different stands at the same time. In the same way, the
#' provided species codes are not required to be the same for each tree.
#'
#' @param species_id Vector of species id's preferably following the
#'   *ger_nfi_2012* species coding. Ideally, these species_id's are provided as
#'   a \code{\link{fe_species_ger_nfi_2012}} object. Second best, they are
#'   provided in the *tum_wwk_short* coding. For any other type of object, an
#'   attempt will be made to convert into *ger_nfi_2012* (and use the
#'   corresponding parameters); if that fails, conversion to *tum_wwk_short*
#'   will be attempted (see details).
#'
#' @param dbh_cm Vector of tree dbh values in cm (dbh = stem diameter at breast
#'   height, i.e. 1.3 m)
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
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   # Three examples for single tree applications with species codes given
#'   # as integers (but following the ger_nfi_2012 coding)
#'
#'   # European beech, dbh_cm < dq_cm
#'   h_standard_gnfi3(species_id = 100, dbh_cm = 14.8, d_q_cm = 25, h_q_m = 22)
#'
#'   # Scots pine, dbh_cm == dq_cm
#'   h_standard_gnfi3(species_id = 20, dbh_cm = 25, d_q_cm = 25, h_q_m = 22)
#'
#'   # Douglas fir, dbh_cm > dq_cm
#'   h_standard_gnfi3(species_id = 40, dbh_cm = 45, d_q_cm = 25, h_q_m = 22)
#'
#'   # Same Douglas fir but species_id = 7 (i.e. tum_wwk_short),
#'   # note the message, because numeric 7 is not convertible into ger_nfi_2012
#'   h_standard_gnfi3(species_id = 7, dbh_cm = 45, d_q_cm = 25, h_q_m = 22)
#'
#'   # But no message, when species_id = 7 is made a tum_wwk_short object first,
#'   # because this can be unambiguously converted into ger_nfi_2012
#'   h_standard_gnfi3(
#'     fe_species_tum_wwk_short(7), dbh_cm = 45, d_q_cm = 25, h_q_m = 22
#'   )
#'
#'   # Usually, applications will be vectorized
#'   species_id <- fe_species_ger_nfi_2012(rep(20, 7))  # Seven Scots pines
#'   dbh <- c(10.1, 27.4, 31.4, 35.5, 39.8, 45.2, 47.2) # and their diameters
#'   # Estimate the heights of these trees, assuming they are from a
#'   # stand with a mean diameter of 35.5 cm, and a corresponding mean height
#'   # of 28 m.
#'   h_standard_gnfi3(species_id, dbh, d_q_cm = 35.5, h_q_m = 28.0)
#'
#'   # Compare with sister function h_standard_bv, assuming a stand age of
#'   # 100 years
#'   h_standard_bv(species_id, dbh, age_yr = 100, d_q_cm = 35.5, h_q_m = 28.0)
#'
h_standard_gnfi3 <- function(species_id, dbh_cm, d_q_cm, h_q_m) {
  UseMethod("h_standard_gnfi3")
}


#' Helper function for cascaded species conversion when applying the German
#' National forest inventory 2012 standard height curve system.
#' Try to convert given species_id into ger_nfi_2012. If this causes an error
#' convert to tum_wwk_short instead
#'
#' @keywords
#'   internal
hstd_gnfi3_spec_convert <- function(species_id) {
  tryCatch(
    list(
      species_id = as_fe_species_ger_nfi_2012(species_id),
      params = h_standard_ger_nfi_2012_param_orig
    ),
    error = function(cnd) {
      message("Using tum_wwk_short species coding for height estimates")
      list(
        species_id = as_fe_species_tum_wwk_short(species_id),
        params = h_standard_ger_nfi_2012_param_tum_wwk_short
      )
    }
  )
}


# This default function should cover most cases. An attempt will be made to
# convert species_id into ger_nfi_2012. If this does not work, an attempt will
# be made to convert it into tum_wwk_short (calling hstd_gnfi3_spec_convert).
# the core function h_standard_gnfi3_core will be then called appropriately

#' @export
h_standard_gnfi3.default <- function(species_id, dbh_cm, d_q_cm, h_q_m) {

  spec_conv  <- hstd_gnfi3_spec_convert(species_id)
  species_id <- vctrs::vec_data(spec_conv$species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m, spec_conv$params
  )
}


# The following three cases cover the situation where species_id are either
# given as ger_nfi_2012, tum_wwk_short or as bavrn_state_short, respectively.
# In these cases, no conversion attempts have to be made.

#' @export
h_standard_gnfi3.fe_species_ger_nfi_2012 <- function(species_id, dbh_cm,
                                                     d_q_cm, h_q_m) {

  species_id <- vctrs::vec_data(species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m,
    params = h_standard_ger_nfi_2012_param_orig
  )
}


#' @export
h_standard_gnfi3.fe_species_tum_wwk_short <- function(species_id, dbh_cm,
                                                      d_q_cm, h_q_m) {

  species_id <- vctrs::vec_data(species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m,
    params = h_standard_ger_nfi_2012_param_tum_wwk_short
  )
}


#' @export
h_standard_gnfi3.fe_species_bavrn_state_short <- function(species_id, dbh_cm,
                                                          d_q_cm, h_q_m) {

  species_id <- vctrs::vec_data(species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m,
    params = h_standard_ger_nfi_2012_param_bavrn_state_short
  )
}


# If species_id is given as bavrn_state, it is converted into bavrn_state_short,
# and called with the appropriate parameters

#' @export
h_standard_gnfi3.fe_species_bavrn_state <- function(species_id, dbh_cm,
                                                    d_q_cm, h_q_m) {

  species_id <- as_fe_species_bavrn_state_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m,
    params = h_standard_ger_nfi_2012_param_bavrn_state_short
  )
}


# If species_id is given as tum_wwk_long, it is converted into tum_wwk_short,
# and called with the appropriate parameters

#' @export
h_standard_gnfi3.fe_species_tum_wwk_long <- function(species_id, dbh_cm,
                                                     d_q_cm, h_q_m) {

  species_id <- as_fe_species_tum_wwk_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  h_standard_gnfi3_core(
    species_id, dbh_cm, d_q_cm, h_q_m,
    params = h_standard_ger_nfi_2012_param_tum_wwk_short
  )
}


#' Core Function for the Estimation of Tree Heights Using the Standard Height
#' Curve System of the 3rd German National Forest Inventory
#'
#' See documentation of \code{\link{h_standard_gnfi3}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user.
#'
#' The function \code{h_standard_gnfi3} is fully vectorized, inputs are recyled
#' according to the tibble rules
#'
#' @param species_id Vector of species ids (see \code{\link{h_standard_gnfi3}}),
#'   but for this core function to work, \code{species_id} must be provided as
#'   \code{character}. This will and should be not checked inside this function,
#'   because if used as intended, that has happened before calling it. The
#'   codings must either follow the *ger_nfi_2012*, the *tum_wwk_short*, or the
#'   *bavrn_state_short* coding.
#'
#' @param dbh_cm Vector of breast height diameters (see
#'   \code{\link{h_standard_gnfi3}})
#'
#' @param d_q_cm The species quadratic mean diameter (cm) in the stand of
#'   interest. (see \code{\link{h_standard_gnfi3}})
#'
#' @param h_q_m The species mean height (m), corresponding to the quadratic mean
#'   diameter in the stand of interest. (see \code{\link{h_standard_gnfi3}})
#'
#' @param params A data frame that defines the function parameters corresponding
#'   to the species coding represented by \code{species_id}
#'
#'
#' @return A vector with estimates of the trees' heights (m) (see
#'   \code{\link{h_standard_gnfi3}})
#'
#' @keywords internal
#'
h_standard_gnfi3_core <- function(species_id,
                                  dbh_cm,
                                  d_q_cm,
                                  h_q_m,
                                  params) {

  # Check if any of the input vectors is longer than dbh_cm and stop with an
  # error if so. Otherwise, dbh_cm would be recycled in some cases, which is
  # undesired. All other possible problems will be adequately handled by
  # tibble::tibble below
  length_in <- c(
    length(species_id), length(d_q_cm), length(h_q_m)
  )
  if(any(length_in > length(dbh_cm))) {
    stop("No input vector must be longer than dbh_cm.")
  }

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    dbh_cm     = dbh_cm,
    d_q_cm     = d_q_cm,
    h_q_m      = h_q_m
  )

  # link data to the appropriate function parameters
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(params) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # estimate the the tree height(s)
  work_dat |> purrr::pmap_dbl(
    .f = function(dbh_cm, d_q_cm, h_q_m, k0, k1) {

      1.3 +
        (h_q_m - 1.3) *
        exp(k0 * (1 - d_q_cm / dbh_cm)) *
        exp(k1 * (1 / d_q_cm - 1 / dbh_cm))

    }
  )
}
