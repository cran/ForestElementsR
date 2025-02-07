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




#' Calculate or Return the Representation Number per ha for the Trees Contained
#' in a Compatible Object
#'
#' The idea behind creating this function was to allow for using the very same
#' evaluation algorithms for ha-based values for a broad range of different
#' objects, e.g. stands/research plots, inventory plots, etc.
#'
#' If the object \code{x} contains information about its area, the
#' representation numbers for each tree will be calculated in the following way:
#'
#' While \code{n_rep_ha} will return a vector of equal numbers for
#' \code{\link{fe_stand}} objects, this is less trivial for the class
#' \code{\link{fe_stand_spatial}}. The latter might contain 'buffer zone trees'
#' beyond the actual stand outline. Such trees will obtain a zero representation
#' number in contrast to the trees inside the outline.
#' If \code{x} does not contain sufficient information about its area, the
#' function will simply hand back the \code{n_rep_ha} column of the
#' \code{x$trees} data frame.
#'
#' @param x An object provided by **ForestElementsR** containing trees for which
#'   a representation number per ha can be meaningfully given. Such an object
#'   must contain a data frame called 'trees'. Typically, this is an
#'   \code{\link{fe_stand}} or \code{\link{fe_stand_spatial}} object.
#'
#' @return A vector of representation numbers corresponding to the order of the
#'   trees data frame in \code{x}.
#'
#' @export
#'
#' @examples
#' # example for an fe_stand object
#' spruce_beech_1_fe_stand |> n_rep_ha()
#'
#' # example for an fe_stand_spatial object
#' mm_forest_1_fe_stand_spatial |> n_rep_ha()
#'
n_rep_ha <- function(x) {
  UseMethod("n_rep_ha")
}


#' @export
n_rep_ha.fe_stand <- function(x) {
  if (is.null(x$area_ha)) {
    return(x$trees$n_rep_ha)
  } else {
    rep(1 / ForestElementsR::get_area_ha(x), nrow(x$trees))
  }
}


#' @export
n_rep_ha.fe_stand_spatial <- function(x) {
  if (is.null(x$outline)) {
    return(x$trees$n_rep_ha)
  } else {
    # Check which trees are inside the stand borders, and which are not
    pos <- x$tree_positions
    pos$.in_ <- sf::st_intersects(pos, x$outline, sparse = FALSE) |> as.vector()
    pos <- sf::st_drop_geometry(pos)

    a_ha <- ForestElementsR::get_area_ha(x)

    # Join with tree data (because one tree can be there with > 1 observations
    index <- c(1:nrow(x$trees)) # for making order preservation 200% safe
    inside <- x$trees |>
      dplyr::select(tidyselect::all_of(c("tree_id", "time_yr"))) |>
      dplyr::left_join(pos, by = "tree_id") |>
      purrr::pluck(".in_")

    # Any tree outside gets zero n_rep
    inside[index] * 1 / a_ha
  }
}



#' @export
n_rep_ha.fe_ccircle_spatial <- function(x) {
  # classify dbh intervals according to concentric circles

  circles_def <- x$circle_definition

  # calcualte slope in radians

  if (!is.null(unique(circles_def$slope))) {
    slope_rad <- unique(circles_def$slope) * (pi / 180)
  } else {
    slope_rad <- 0
  }

  # classify trees according to area
  trees_classified <- transform(
    x$trees,
    c_area = circles_def$c_area[findInterval(
      x$trees$dbh_cm,
      circles_def$dbh_lower
    )]
  )

  # correct area with slope
  trees_classified <- trees_classified |>
    dplyr::mutate(c_area_slope_corr = .data$c_area *
      cos(slope_rad))


  # estimate representation area (1/circle area)
  1 / trees_classified$c_area_slope_corr
}
