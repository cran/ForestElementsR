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



#' @import sf
#' @importFrom rlang .data
#' @importFrom Rdpack reprompt
#' @importFrom tidyselect vars_select_helpers
NULL

# 'wood_density_tum_wwk_short' is an exported package dataset that is referenced
# by its bare name inside species_shares(). Declaring it here prevents a
# spurious "no visible binding for global variable" NOTE from R CMD check.
utils::globalVariables("wood_density_tum_wwk_short")
