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




#' Estimate a tree's crown diameter
#'
#' This function can be used for estimating a tree's crown diameter, given its
#' species, its stem diameter at breast height, and its total height. This is
#' the crown diameter function which is implemented in the forest growth
#' simulator SILVA \insertCite{pretzsch_single_2002}{ForestElementsR}. The crown
#' diameter in this context is defined as the average diameter of the crown at
#' its greatest lateral extension. The crown diameter equations are
#' available for exactly the species (groups) defined in the coding
#' *tum_wwk_short*. If they are called with another species coding supported by
#' the package **ForestElementsR**, \code{crown_diameter_silva} will attempt to
#' convert them accordingly.
#'
#' @param species_id Vector of species id's following the *tum_wwk_short*
#'   species coding. Ideally, these species_id's are provided as a
#'   \code{fe_species_tum_wwk_short} object. If they are provided as another
#'   object, \code{crown_diameter_silva} will make an attempt to convert them.
#'   If this is not possible, the function will terminate with an error. The
#'   species id's can also be provided as numeric values (\code{double} or
#'   \code{integer}) or \code{character}. These will be internally converted to
#'   \code{fe_species_tum_wwk_short}. If this fails (i.e. the user provided
#'   species codes not defined in the *tum_wwk_short* coding), an error is
#'   thrown and the function terminates.
#'
#' @param dbh_cm Vector of tree dbh values in cm (dbh = stem diameter at breast
#'   height, i.e. 1.3 m)
#'
#' @param height_m Vector of tree height values in m
#'
#' @return An estimate of the tree's diameter of the crown at its greatest
#'   lateral extension in m.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' # Estimate the crown diameter of a Scots pine with a stem diameter
#' # at breast height of 45.2 cm and a total height of 29.2 m:
#' crown_diameter_silva(
#'   species_id = "3",    # will be internally converted to tum_wwk_short
#'   dbh_cm = 45.2,
#'   height_m = 29.2
#' ) # 6.1 m (rounded)
#'
#' # Crown diameter estimate for a European beech with
#' # the same height and diameter:
#' crown_diameter_silva(
#'   species_id = "5",   # will be internally converted to tum_wwk_short
#'   dbh_cm = 45.2,
#'   height_m = 29.2
#' ) # 9.6 m (rounded)
#'
#' # Run vectorized
#' spec <- mm_forest_1_fe_stand_spatial$trees$species_id
#' d <- mm_forest_1_fe_stand_spatial$trees$dbh_cm
#' h <- mm_forest_1_fe_stand_spatial$trees$height_m
#' crown_diameter_silva(spec, d, h)
#'
crown_diameter_silva <- function(species_id, dbh_cm, height_m) {
  UseMethod("crown_diameter_silva")
}


# crown_diameter_silva.default applies to all cases, where species_id is not
# given as n tum_wwk_short object. It makes a conversion attempt and calculates
# the crown diameters if the conversion is successful.

#' @export
crown_diameter_silva.default <- function(species_id = species_id,
                                         dbh_cm = dbh_cm,
                                         height_m = height_m) {
  species_id <- vctrs::vec_data(as_fe_species_tum_wwk_short(species_id))
  crown_diameter_silva_core(species_id, dbh_cm, height_m)
}


# If species_id is already given as a tum_wwk_short object, no conversion
# is required

#' @export
crown_diameter_silva.fe_species_tum_wwk_short <- function(species_id,
                                                          dbh_cm,
                                                          height_m) {
    species_id <- vctrs::vec_data(species_id)
    crown_diameter_silva_core(species_id, dbh_cm, height_m)
}


#' Core function for the silva tree crown diameter calculation
#'
#' See documentation of \code{\link{crown_diameter_silva}} for most information,
#' this function is the workhorse behind, and should never be called directly by
#' a user.
#'
#' The function \code{crown_diameter_silva} is fully vectorized, inputs are
#' recycled according to the tibble rules
#'
#' @param species_id Vector of species ids
#' (see \code{\link{crown_diameter_silva}}), but for this core function to work,
#' \code{species_id} must be provided as \code{character}. This will and should
#' be not checked inside this function, because if used as intended, that has
#' happened before calling it.
#'
#' @param dbh_cm Vector of breast height diameters
#' (see \code{\link{crown_diameter_silva}})
#'
#' @param height_m Vector of tree heights
#' (see \code{\link{crown_diameter_silva}})
#'
#' @return Tree crown diameter (see \code{\link{crown_diameter_silva}})
#'
#' @keywords internal
#'
crown_diameter_silva_core <- function(species_id, dbh_cm, height_m) {
  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    dbh_cm     = dbh_cm,
    height_m   = height_m
  )

  # link data to the function parameters stored in cd_param_silva
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(cd_param_silva) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # calculate the crown diameter(s)
  # cd = exp(a0 + a1 * log(dbh_cm) + a2 * height_m + a3 * log(height_m/dbh_cm))
  work_dat |> purrr::pmap_dbl(
    .f = function(dbh_cm, height_m, a0, a1, a2, a3) {
      exp(
        a0 +
          a1 * log(dbh_cm) +
          a2 * height_m +
          a3 * log(height_m / dbh_cm)
      )
    }
  )
}
