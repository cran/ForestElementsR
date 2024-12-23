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




#' Plot an fe_ccircle_spatial_notrees Object
#'
#' For an object of class\code{\link{fe_ccircle_spatial_notrees}} only the
#' concentric circles are plotted at their correct positions.
#'
#' @param x An \code{\link{fe_ccircle_spatial_notrees}} object
#'
#' @param ... Additional arguments, not used in
#'   \code{plot.fe_ccircle_spatial_notrees}
#'
#' @return A map (ggplot2) of the plot layout including the trees with
#'   coordinates
#'
#' @export
#'
#' @examples
#'   # Abuse input data, that would actually allow for a full fe_ccircle_spatial
#'   # object to construct a fe_ccircle_spatial_notrees object.
#'   x <- spruce_pine_ccircle_raw
#'   x <- x |> fe_ccircle_spatial_notrees()
#'
#'   # Plot it
#'   plot(x)
#'
plot.fe_ccircle_spatial_notrees <- function(x, ...) {

  circles <- x$circle_definition
  circles$r <- sqrt(circles$c_area * 10000 / pi)

  buffer_circles <- st_buffer(
    circles,
    sqrt(circles$c_area * 10000 / pi)
  )

  image <- buffer_circles |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      color = "black",
      alpha = 0
    ) +
    ggplot2::labs(title = x$stand_id)

  # Plot axes labels are not necessary for georeferenced coordinates
  if (is.na(sf::st_crs(x$circle_definition))) {
    image <- image +
      ggplot2::scale_x_continuous("x (m)") +
      ggplot2::scale_y_continuous("y (m)")
  }

  image
}






#' summary For a fe_ccircle_spatial_notrees object
#'
#' For the time being, this is function serves only to obtain a controled
#' output when \code{\link{summary}} is called for an object of class
#' \code{\link{fe_ccircle_spatial_notrees}}. This output is just an empty
#' data data frame.
#'
#' @param object An \code{\link{fe_ccircle_spatial_notrees}} object
#'
#' @param ... Additional arguments, not used in
#'   \code{plot.fe_ccircle_spatial_notrees}
#'
#' @return An empty data frame
#'
#' @export
#'
#' @examples
#'   # Abuse input data, that would actually allow for a full fe_ccircle_spatial
#'   # object to construct a fe_ccircle_spatial_notrees object.
#'   x <- spruce_pine_ccircle_raw
#'   x <- x |> fe_ccircle_spatial_notrees()
#'
#'   # Make the dummy summary
#'   summary(x)
#'
summary.fe_ccircle_spatial_notrees <- function(object, ...) {
  message("Returning empty data frame for class fe_ccircle_spatial_notrees")
  data.frame()
}





