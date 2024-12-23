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




#' Get the Area in ha of a Compatible Object
#'
#' The function \code{get_area_ha} will return the correct area for all
#' appropriate objects provided by **ForestElementsR**. While the
#' \code{\link{fe_stand}} class contains this information directly, this is not
#' the case for the \code{\link{fe_stand_spatial}} class. However,
#' \code{get_area_ha} will work for both (and other objects to be implemented)
#' in the same way. In case the object \code{x} does not contain sufficient
#' information, the function returns NULL.
#'
#' @param x Object provided by **ForestElementsR** for which an area can be
#'   meaningfully given, typically \code{\link{fe_stand}} or
#'   \code{\link{fe_stand_spatial}}
#'
#' @return The object's area in ha. In case the object \code{x} does not contain
#'   sufficient information, the function returns NULL.
#'
#' @export
#'
#' @examples
#' # Example for an fe_stand object
#' selection_forest_1_fe_stand |> get_area_ha()
#'
#' # Example for an fe_stand_spatial object
#' mm_forest_1_fe_stand_spatial |> get_area_ha()
#'
get_area_ha <- function(x) {
  UseMethod("get_area_ha")
}


# For an fe_stand_spatial object, the area is calculated from the outline
# As coordinates must be given in m, the resulting area is in m² which must be
# divided by 10000 in order to obtain ha

#' @export
get_area_ha.fe_stand_spatial <- function(x) {
  if (is.null(x$outline)) {
    return(NULL)
  } else {
    sf::st_area(x$outline) / 10000
  }
}


# In case of an fe_stand object, the area is directly requested from the object

#' @export
get_area_ha.fe_stand <- function(x) {
  x$area_ha
}


# For an fe_ccircle_spatial object, the area is calculated from the area of
# the outer circle

#' @export
get_area_ha.fe_ccircle_spatial <- function(x) {
  ccircles <- x[["circle_definition"]]

  max(ccircles$c_area)
}
