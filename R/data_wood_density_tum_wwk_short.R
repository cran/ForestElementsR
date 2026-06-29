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




#' Basic Wood Density Values for the *tum_wwk_short* Species Coding
#'
#' @description Basic wood density (oven-dry mass over green volume) for each
#'   species of the *tum_wwk_short* species coding (see
#'   \code{\link{species_codings}}). These values are used for converting wood
#'   volumes into masses, e.g. when calculating species shares by biomass.
#'
#' @format A tibble with one row per species of the *tum_wwk_short* coding and
#'   the columns:
#' \describe{
#'   \item{species_id}{the species code, an object of class
#'     \code{fe_species_tum_wwk_short}}
#'   \item{wood_density}{basic wood density in \eqn{t/m^3} (i.e. oven-dry mass
#'     per unit of green volume)}
#' }
#'
#' @examples
#' wood_density_tum_wwk_short
#'
"wood_density_tum_wwk_short"
