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




#' Obtaining Center Coordinates of Spatial \code{fe_stand} Child Objects
#'
#' @param x An object of one of \code{fe_stand}'s child classes that carry
#'   spatial information, i.e. \code{\link{fe_stand_spatial}},
#'   \code{\link{fe_ccircle_spatial}}, \code{\link{fe_ccircle_spatial_notrees}}
#'
#' @returns The coordinates of the point that describes best the spatial center
#'   of the object \code{x}. In case of objects of classes
#'   \code{\link{fe_ccircle_spatial}}, and
#'   \code{\link{fe_ccircle_spatial_notrees}}, this is the center of the
#'   concentric circle with the largest area. In case of an
#'   \code{\link{fe_stand_spatial}} object, this is the center of gravity of the
#'   object's polygon outline. The point object returned belongs to a class
#'   provided by the package \code{sf}. The class of that object depends
#'   on how the coordinates are defined in \code{x} (i.e. as an
#'   \code{sfc} or an \code{sfg} object). If \code{x} is unsuitable for
#'   retrieving center coordinates, the function returns an empty geometry.
#'
#' @export
#'
#' @examples
#'   spruce_pine_ccircle_spatial_notrees |> get_center()
#'   spruce_pine_ccircle_spatial_notrees |> get_center()
#'   mm_forest_1_fe_stand_spatial |> get_center()
#'
#'   # Unsuitable objects
#'   get_center(spruce_beech_1_fe_stand)
#'   get_center("this is a character string")
#'
get_center <- function(x) {
  UseMethod("get_center")
}


#' @export
get_center.fe_ccircle_spatial <- function(x) {
  # Returns midpoint of the largest circle,
  # also covers fe_ccircle_spatial_notrees which
  # is a child of fe_ccircle spatial
  i <- which.max(x$circle_definition$c_area)
  sf::st_geometry(x$circle_definition[i, ])
}


#' @export
get_center.fe_stand_spatial <- function(x) {
  # Polygon's Center of Gravity
  sf::st_centroid(x$outline)
}


#' @export
get_center.default <- function(x) {
  # We get here only in case of unsuitable input objects
  # Issue a warning and return an empty geometry
  warning(
    paste0("Function get_center() is not applicabe to objects of class ",
           class(x)[1], ". Returning an empty geometry.")
  )

  sf::st_sfc(NA)
}





