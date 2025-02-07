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




#' calculate tree basal area in cm2 (ba_cm2)
#'
#' @param dbh tree diameter in cm
#'
#' @noRd
tree_ba_cm2 <- function(dbh) {
  ba_cm2 <- pi / 4 * (dbh)^2

  return(ba_cm2)
}



#' calculate tree basal area in m2 (ba_m2)
#'
#' @param dbh tree diameter in cm
#'
#' @noRd
tree_ba_m2 <- function(dbh) {
  ba_cm2 <- pi / 4 * (dbh / 100)^2

  return(ba_cm2)
}
