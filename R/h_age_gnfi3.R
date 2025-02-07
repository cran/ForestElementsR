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




#' Estimate Tree Height Growth With the 3rd German National Forest Inventory
#' Growth Model (2012)
#'
#' Tree height growth model of the third German National Forest Inventory of
#' 2012 \insertCite{bwi3_methods_2017}{ForestElementsR}. Allows to estimate a
#' tree's height at any age if its height is known at a given age.
#'
#' Originally, the function was parameterized for species and species groups
#' corresponding to the national forest inventory's species coding
#' (\code{\link{fe_species_ger_nfi_2012}}). We have attributed in addition these
#' the original parameters also to the species codings
#' \code{\link{fe_species_tum_wwk_short}}, and
#' \code{\link{fe_species_bavrn_state_short}}. When called with a given species
#' coding, the function will try to use the "nearest" of these three
#' alternatives. Fallback option is the attempt to use
#' \code{\link{fe_species_tum_wwk_short}}.
#'
#' @param species_id Vector of species id's preferably following the
#'   *ger_nfi_2012* species coding. Ideally, these species_id's are provided as
#'   a \code{\link{fe_species_ger_nfi_2012}} object. See Details for how other
#'   species codings are handled.
#'
#' @param age_yr Single numeric value or vector of ages (in years) for which the
#'   height is to be calculated
#'
#' @param h_m_known Vector of known height (m) values at age
#'   \code{age_yr_known}
#'
#' @param age_yr_known Vector of ages (years) for which the height
#'   \code{h_m_known} is known
#'
#' @return A single height value or vector of height values corresponding to
#'   \code{age_yr}
#'
#' @family growth functions
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   # A European beech tree has a height of 25.2 m at age 75. Estimate
#'   # its height at age 83
#'   h_age_gnfi3(100, 83, 25.2, 75) # 100 is ger_nfi_2012 code for E. beech
#'
#'   # Do the same backward in time, height at age 67
#'   h_age_gnfi3(100, 67, 25.2, 75)
#'
#'   # Apply for more than one tree, different species, same age
#'   h_known   <- c(23.1, 16.2, 35.2, 19.3, 21.8)
#'   species   <- as_fe_species_tum_wwk_short(c(3, 3, 3, 6, 6))
#'   h_age_gnfi3(
#'     species, age_yr = 70, h_m_known = h_known, age_yr_known = 60
#'   )
#'
h_age_gnfi3 <- function(species_id, age_yr, h_m_known, age_yr_known) {
  UseMethod("h_age_gnfi3")
}


#' Helper function for cascaded species conversion when applying the function
#' h_age_gnfi3().
#' Try to convert given species_id into ger_nfi_2012. If this causes an error
#' convert to tum_wwk_short instead
#'
#' @keywords
#'   internal
h_age_gnfi3_spec_convert <- function(species_id) {
  tryCatch(
    list(
      species_id = as_fe_species_ger_nfi_2012(species_id),
      params = param_h_age_gnfi_2012_orig
    ),
    error = function(cnd) {
      message("Using tum_wwk_short species coding for height growth estimates")
      list(
        species_id = as_fe_species_tum_wwk_short(species_id),
        params = param_h_age_gnfi_2012_tum_wwk_short
      )
    }
  )
}


# This default function should cover most cases. An attempt will be made to
# convert species_id into ger_nfi_2012. If this does not work, an attempt will
# be made to convert it into tum_wwk_short (calling h_age_gnfi3_spec_convert).
# the core function h_age_gnfi3_core will be then called appropriately

#' @export
h_age_gnfi3.default <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  spec_conv  <- h_age_gnfi3_spec_convert(species_id)
  species_id <- vctrs::vec_data(spec_conv$species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known, spec_conv$params
  )
}


# The following three cases cover the situation where species_id are either
# given as ger_nfi_2012, tum_wwk_short or as bavrn_state_short, respectively.
# In these cases, no conversion attempts have to be made.

#' @export
h_age_gnfi3.fe_species_ger_nfi_2012 <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  species_id <- vctrs::vec_data(species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known,
    params = param_h_age_gnfi_2012_orig
  )
}


#' @export
h_age_gnfi3.fe_species_tum_wwk_short <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  species_id <- vctrs::vec_data(species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known,
    params = param_h_age_gnfi_2012_tum_wwk_short
  )
}


#' @export
h_age_gnfi3.fe_species_bavrn_state_short <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  species_id <- vctrs::vec_data(species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known,
    params = param_h_age_gnfi_2012_bavrn_state_short
  )
}


# If species_id is given as bavrn_state, it is converted into bavrn_state_short,
# and called with the appropriate parameters

#' @export
h_age_gnfi3.fe_species_bavrn_state <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  species_id <-as_fe_species_bavrn_state_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known,
    params = param_h_age_gnfi_2012_bavrn_state_short
  )
}


# If species_id is given as tum_wwk_long, it is converted into tum_wwk_short,
# and called with the appropriate parameters

#' @export
h_age_gnfi3.fe_species_tum_wwk_long <- function(
    species_id, age_yr, h_m_known, age_yr_known
) {

  species_id <- as_fe_species_tum_wwk_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  h_age_gnfi3_core(
    species_id, age_yr, h_m_known, age_yr_known,
    params = param_h_age_gnfi_2012_tum_wwk_short
  )
}



#' Core Function for Estimate Tree Height Growth With the 3rd German National
#' Forest Inventory Growth Model (2012)
#'
#' See documentation of \code{\link{h_age_gnfi3}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user.
#'
#' The function \code{h_age_gnfi3} is fully vectorized, inputs are recyled
#' according to the tibble rules
#'
#' @param species_id Vector of species ids (see \code{\link{d_age_gnfi3}}),
#'   but for this core function to work, \code{species_id} must be provided as
#'   \code{character}. This will and should be not checked inside this function,
#'   because if used as intended, that has happened before calling it. The
#'   codings must either follow the *ger_nfi_2012* or the *tum_wwk_short*
#'   coding. In the former case, the parameter \code{param_orig} must be TRUE,
#'   in the latter, it must be FALSE.
#'
#' @param age_yr Single numeric value or vector of ages (in years) for which the
#'   height is to be calculated
#'
#' @param h_m_known Vector of known height (m) values at age
#'   \code{age_yr_known}
#'
#' @param age_yr_known Vector of ages (years) for which the height
#'   \code{h_m_known} is known
#'
#' @param params A data frame that defines the function parameters corresponding
#'   to the species coding represented by \code{species_id}
#'
#' @return A vector with estimates of the trees' expected heights (see
#'   \code{\link{h_age_gnfi3}})
#'
#' @keywords internal
#'
h_age_gnfi3_core <- function(
    species_id, age_yr, h_m_known, age_yr_known, params
) {

  # Check if any of the input vectors is longer than h_m_known and terminate
  # with an error if so. Otherwise, h_m_known would be recycled in some
  # cases, which is undesired. All other possible problems will be adequately
  # handled by tibble::tibble below
  length_in <- c(
    length(species_id), length(age_yr), length(h_m_known),
    length(age_yr_known)
  )
  if(any(length_in > length(h_m_known))) {
    stop("No input vector must be longer than h_m_known.")
  }

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id   = species_id,
    age_yr       = age_yr,
    h_m_known    = h_m_known,
    age_yr_known = age_yr_known,
  )

  # link data to the appropriate function parameters
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(params) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # estimate the height(s)
  work_dat |> purrr::pmap_dbl(
    .f = function(age_yr, h_m_known, age_yr_known, alpha, beta, gamma) {

      gamma * (h_m_known / gamma) ^ exp(
        - beta / alpha * (age_yr ^ alpha - age_yr_known ^ alpha)
      )

    }
  )
}


