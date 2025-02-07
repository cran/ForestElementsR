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




#' Attribute Tree Heights to Layers After Ernst Assmann
#'
#' Tree heights are attributed to three layers as proposed by
#' \insertCite{assmann_1961;textual}{ForestElementsR}. The layers are called
#' Top, T, Middle, M, and Bottom, B, and correspond to >80%, >50%, and >0% of
#' a reference height (usually the height of the highest tree in the stand of
#' interest).
#'
#' @param heights Vector of tree heights
#'
#' @param reference_height Reference height for the 100% level. If \code{NULL}
#'   (default), the maximum of \code{heights} will be used as reference height.
#'
#' @return An ordered factor of T, M, B values, corresponding to
#'   \code{heights} in the order as \code{heights} was provided.
#'
#' @family structure and diversity
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   # Monospecific stand
#'   trees <- norway_spruce_1_fe_stand$trees
#'   assmann_layers(trees$height_m)
#'
#'   # Selection forest
#'   trees <- selection_forest_1_fe_stand$trees
#'   assmann_layers(trees$height_m)
#'
assmann_layers <- function(heights, reference_height = NULL) {

  if (is.null(reference_height)) reference_height <- max(heights)

  rel_heights <- heights / reference_height

  cut(
    rel_heights,
    breaks = c(0, 0.5, 0.8, max(1, max(rel_heights) * 0.1)),
    labels = c("B", "M", "T"),
    right = TRUE,
    include.lowest = FALSE,
    ordered_result = TRUE
  )
}


