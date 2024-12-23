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




#' Calculate Tree Volumes With the GRI Volume Equations
#' \insertCite{franz_gri_methods_1973}{ForestElementsR}
#'
#' Merchantable standing tree volumes over bark calculated with the GRI volume
#' equations developed by Friedrich Franz in 1971. These volume equations are
#' standard in the German Federal State of Bavaria
#'
#' The abbreviation *GRI* stands for the German word "Großrauminventur" (large
#' area inventory). This forest inventory was conducted in 1971 by Friedrich
#' Franz and his team of the Chair for Forest Growth and Yield Science at the
#' Munich Ludwig-Maximilians-University
#' \insertCite{franz_gri_methods_1973}{ForestElementsR}. The inventory covered
#' the whole federal state of Bavaria (~ 70,600 km²). The volume equations
#' implemented in this function were calibrated with the data of several ten
#' thousands of trees which were felled for that purpose during the
#' inventory. The volume equations are available for exactly the species
#' (groups) defined in the coding *tum_wwk_short*. If they are called with
#' another species coding supported by the package **ForestElementsR**,
#' \code{v_gri} will attempt to convert them accordingly.
#'
#'
#' @param species_id Vector of species id's following the *tum_wwk_short*
#'   species coding. Ideally, these species_id's are provided as an
#'   \code{\link{fe_species_tum_wwk_short}} or an
#'   \code{\link{fe_species_bavrn_state_short}} object. If they are provided as
#'   another \code{fe_species} object, \code{v_gri} will make an attempt to
#'   convert them into \code{\link{fe_species_tum_wwk_short}}. The exception is
#'   the coding \code{\link{fe_species_bavrn_state}} which will be converted
#'   into \code{\link{fe_species_bavrn_state_short}}. If all conversion attempts
#'   fail, the function will terminate with an error. The species id's can also
#'   be provided as numeric values (\code{double} or \code{integer}) or
#'   \code{character}. These will be internally converted to
#'   \code{\link{fe_species_tum_wwk_short}}. If this fails (i.e. the user
#'   provided species codes are not defined in the *tum_wwk_short* coding), an
#'   error is thrown and the function terminates.
#'
#' @param dbh_cm Vector of tree dbh values in cm (dbh = stem diameter at breast
#'   height, i.e. 1.3 m)
#'
#' @param height_m Vector of tree height values in m
#'
#' @return A vector of merchantable standing tree wood volumes over bark in m³.
#'   "Merchantable" means only wood with a minimum diameter of 7 cm over bark is
#'   taken into account. Therefore, for small trees without any merchandable
#'   wood, the function will return 0 m³.
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
#'
#' @examples
#' # Find out the species codes that work with v_gri
#' fe_species_get_coding_table("tum_wwk_short") |>
#'   dplyr::select(-genus, -species_no) |>
#'   dplyr::distinct()
#'
#' # Merchantable volume of a European beech with dbh = 30 cm,
#' # and height = 29 m
#' v_gri("5", 30, 29)
#' v_gri(5, 30, 29)
#' v_gri(as_fe_species_tum_wwk_short(5), 30, 29)
#'
#' # Several trees (three species, three sizes)
#' species_id <- fe_species_tum_wwk_short(c(1, 1, 1, 3, 3, 3, 5, 5, 5))
#' dbh_cm <- c(12, 30, 55, 12, 30, 55, 12, 30, 55)
#' height_m <- c(14, 33, 39, 14, 33, 39, 14, 33, 39)
#' v_gri(species_id, dbh_cm, height_m)
#'
#' # The same, but the species id's are now originally defined in the
#' # coding of the 2012 German national forest inventory
#' species_id <- fe_species_ger_nfi_2012(
#'   c(10, 10, 10, 20, 20, 20, 100, 100, 100)
#' )
#' v_gri(species_id, dbh_cm, height_m)
#'
v_gri <- function(species_id, dbh_cm, height_m) {
  UseMethod("v_gri")
}


# v_gri.default applies to almost all cases, where species_id is not given as an
# tum_wwk_short object. It makes a conversion attempt and calculates the
# the volumes if the conversion is successful.

#' @export
v_gri.default <- function(species_id = species_id,
                          dbh_cm = dbh_cm,
                          height_m = height_m) {
  species_id <- vctrs::vec_data(as_fe_species_tum_wwk_short(species_id))
  v_gri_core(species_id, dbh_cm, height_m, v_gri_param_orig)
}


# If species_id is given as a bavrn_state object, it is converted into
# bavrn_state_short (for which a specific parameter set is provided), and
# calculates the volumes

#' @export
v_gri.fe_species_bavrn_state <- function(species_id, dbh_cm, height_m) {
  species_id <- vctrs::vec_data(as_fe_species_bavrn_state_short(species_id))
  v_gri_core(species_id, dbh_cm, height_m, v_gri_param_bv_st_shrt)
}


# If species_id is already given as a tum_wwk_short object, no conversion
# is required

#' @export
v_gri.fe_species_tum_wwk_short <- function(species_id, dbh_cm, height_m) {
  species_id <- vctrs::vec_data(species_id)
  v_gri_core(species_id, dbh_cm, height_m, v_gri_param_orig)
}

# If species_id is already given as a bavrn_state_short object, no conversion
# is required, because there is an own parameter set

#' @export
v_gri.fe_species_bavrn_state_short <- function(species_id, dbh_cm, height_m) {
  species_id <- vctrs::vec_data(species_id)
  v_gri_core(species_id, dbh_cm, height_m, v_gri_param_bv_st_shrt)
}



#' Core Function For the GRI Tree Volume Calculation
#'
#' See documentation of \code{\link{v_gri}} for most information, this function
#' is the workhorse behind, and should never be called directly by a user.
#'
#' The function \code{v_gri} is fully vectorized, inputs are recyled according
#' to the tibble rules
#'
#' @param species_id Vector of species ids (see \code{\link{v_gri}}), but for
#'   this core function to work, \code{species_id} must be provided as
#'   \code{character}. This will and should be not checked inside this function,
#'   because if used as intended, that has happened before calling it.
#'
#' @param dbh_cm Vector of breast height diameters (see \code{\link{v_gri}})
#' @param height_m Vector of tree heights (see \code{\link{v_gri}})
#'
#' @param v_param An internally provided object with appropriate species (group)
#'   specific paramters
#'
#' @return Tree wood volume (see \code{\link{v_gri}})
#'
#' @keywords internal
#'
v_gri_core <- function(species_id, dbh_cm, height_m, v_param) {

  # Check for missing values in the arguments and terminate if there are any
  if (any(
    c(any(is.na(species_id)), any(is.na(dbh_cm)), any(is.na(height_m)))
  )) {
    stop("No missing values allowed in arguments to v_gri()")
  }

  # get data in a workable format
  work_dat <- tibble::tibble(
    species_id = species_id,
    dbh_cm     = dbh_cm,
    height_m   = height_m
  )

  # link data to the function parameters stored in v_gri_param
  work_dat <- suppressMessages(
    work_dat |>
      dplyr::left_join(v_param) |>
      dplyr::select(-species_id) # cumbersome if kept from here
  )

  # calculate the volume(s)
  work_dat |> purrr::pmap_dbl(
    .f = function(dbh_cm, height_m, C) { # Using capital C as in Franz' notation
      if ((dbh_cm < 6.1) | (height_m < 1.3)) {
        return(0) # trees below a certain size have 0 m^3 of merchantable volume
      } else {
        # Franz' equation system; using capital C and A as in Franz' notation
        dbh_vec <- c(1, log(dbh_cm), log(dbh_cm)^2)
        A <- t(dbh_vec) %*% C
        h_vec <- c(1, log(height_m), log(height_m)^2)
        form_h <- exp(A %*% h_vec) # form height
        max(0, pi / 40000 * dbh_cm^2 * form_h)
      }
    }
  )
}
