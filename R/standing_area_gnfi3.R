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




#' Estimate the Standing Area of Single Trees
#'
#' An implementation of the standing area estimation of the third German
#' National Forest Inventory \insertCite{bwi3_methods_2017}{ForestElementsR}.
#' Its main intended use is the calculation of virtual species areas in mixed
#' stands. According to \insertCite{bwi3_methods_2017}{ForestElementsR}, it is
#' recommended only to include the main stand in such calculations, neither
#' understorey, nor any layers above the main stand.
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
#' @param dbh_cm Vector of tree dbh values in cm (dbh = stem diameter at breast
#'   height, i.e. 1.3 m)
#'
#' @return A vector of the estimated standing areas in m²
#'
#' @export
#'
#' @examples
#'   # Three spruces, two pines, two beech
#'   species_id <- fe_species_ger_nfi_2012(c(10, 10, 10, 20, 20, 100, 100))
#'   dbh_cm     <- c(10.1, 27.4, 31.4, 35.5, 39.8, 45.2, 47.2)
#'
#'   standing_area_gnfi3(species_id, dbh_cm)
#'
standing_area_gnfi3 <- function(species_id, dbh_cm) {
  UseMethod("standing_area_gnfi3")
}



#' Helper function for cascaded species conversion when applying the standing
#' area estimation of the German National forest inventory 2012.
#' Try to convert given species_id into ger_nfi_2012. If this causes an error
#' convert to tum_wwk_short instead (to be used in the default version of the
#' function).
#'
#' @keywords
#'   internal
stndg_area_gnfi3_spec_convert <- function(species_id) {
  tryCatch(
    list(
      species_id = as_fe_species_ger_nfi_2012(species_id),
      params = param_standing_area_gnfi2012_orig
    ),
    error = function(cnd) {
      message("Using tum_wwk_short species coding for standing area estimates")
      list(
        species_id = as_fe_species_tum_wwk_short(species_id),
        params = param_standing_area_gnfi2012_tum_short
      )
    }
  )
}


# This default function should cover most cases. An attempt will be made to
# convert species_id into ger_nfi_2012. If this does not work, an attempt will
# be made to convert it into tum_wwk_short (calling
# stndg_area_gnfi3_spec_convert). The core function standing_area_gnfi3_core
# will be then called appropriately

#' @export
standing_area_gnfi3.default <- function(species_id, dbh_cm) {

  spec_conv  <- stndg_area_gnfi3_spec_convert(species_id)
  species_id <- vctrs::vec_data(spec_conv$species_id)

  standing_area_gnfi3_core(
    species_id, dbh_cm, spec_conv$params
  )
}


# The following three cases cover the situation where species_id are either
# given as ger_nfi_2012, tum_wwk_short or as bavrn_state_short, respectively.
# In these cases, no conversion attempts have to be made.

#' @export
standing_area_gnfi3.fe_species_ger_nfi_2012 <- function(species_id, dbh_cm) {

  species_id <- vctrs::vec_data(species_id)
  # Straightforward: If species are given as ger_nfi_2012, call the core
  # function with param_orig = TRUE
  standing_area_gnfi3_core(
    species_id, dbh_cm, params = param_standing_area_gnfi2012_orig
  )
}


#' @export
standing_area_gnfi3.fe_species_tum_wwk_short <- function(species_id, dbh_cm) {

  species_id <- vctrs::vec_data(species_id)
  # Straightforward: If species are given as tum_wwk_short, call the core
  # function with the appropriate parameters
  standing_area_gnfi3_core(
    species_id, dbh_cm, params = param_standing_area_gnfi2012_tum_short
  )
}


#' @export
standing_area_gnfi3.fe_species_bavrn_state_short <- function(species_id,
                                                             dbh_cm) {
  species_id <- vctrs::vec_data(species_id)
  # Straightforward: If species are given as bavrn_state_short, call the core
  # function with tha appropriate pameters
  standing_area_gnfi3_core(
    species_id, dbh_cm, params = param_standing_area_gnfi2012_bavrn_short
  )
}


# If species_id is given as bavrn_state, it is converted into bavrn_state_short,
# and called with the appropriate parameters

#' @export
standing_area_gnfi3.fe_species_bavrn_state <- function(species_id, dbh_cm) {

  species_id <- as_fe_species_bavrn_state_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  standing_area_gnfi3_core(
    species_id, dbh_cm, params = param_standing_area_gnfi2012_bavrn_short
  )
}

# If species_id is given as tum_wwk_long, it is converted into tum_wwk_short,
# and called with the appropriate parameters

#' @export
standing_area_gnfi3.fe_species_tum_wwk_long <- function(species_id, dbh_cm) {

  species_id <- as_fe_species_tum_wwk_short(species_id)
  species_id <- vctrs::vec_data(species_id)

  standing_area_gnfi3_core(
    species_id, dbh_cm, params = param_standing_area_gnfi2012_tum_short
  )
}


#' Core Function for the Estimation of Tree Standing Areas According to the 3rd
#' German National Forest Inventory
#'
#' See documentation of \code{\link{standing_area_gnfi3}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user. The function \code{standing_area_gnfi3} is fully vectorized, inputs
#' are recyled according to the tibble rules
#'
#' @param species_id Vector of species ids (see
#'   \code{\link{standing_area_gnfi3}}), but for this core function to work,
#'   \code{species_id} must be provided as \code{character}. This will and
#'   should be not checked inside this function, because if used as intended,
#'   that has happened before calling it. The codings must either follow the
#'   *ger_nfi_2012*, the *tum_wwk_short*, or the *bavrn_state_short* coding.
#'
#' @param dbh_cm Vector of breast height diameters (see
#'   \code{\link{standing_area_gnfi3}})
#'
#' @param params A data frame that defines the function parameters corresponding
#'   to the species coding represented by \code{species_id}
#'
#' @return A vector with estimates of the trees' standing areas in m²
#'
#' @keywords internal
#'
standing_area_gnfi3_core <- function(species_id, dbh_cm, params) {

  # Check if any of the input vectors is longer than dbh_cm and stop with an
  # error if so. Otherwise, dbh_cm would be recycled in some cases, which is
  # undesired. All other possible problems will be adequately handled by
  # tibble::tibble below
  if(length(species_id) > length(dbh_cm)) {
    stop("Input vector `species_id` must not be longer than dbh_cm.")
  }

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    dbh_cm     = dbh_cm
  )

  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(params) |>
      dplyr::select(!"species_id" & !"max_n_ha")
  )

  # the actual calculation
  work_dat |> purrr::pmap_dbl(
    .f = function(dbh_cm = dbh_cm, alpha = alpha, beta = beta) {
      alpha + beta * (dbh_cm / 100) ^ 2 * pi / 4
    }
  )
}


