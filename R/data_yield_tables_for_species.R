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




#' Yield Tables To Species Assignments
#'
#' @description In order to facilitate the application of yield tables, we
#'   provide data frames that link the names of implemented
#'   \code{\link{fe_yield_table}} objects to species codings. Currently, there
#'   are three such data frames:
#'   \code{\link{fe_species_tum_wwk_short}},
#'   \code{\link{fe_species_bavrn_state_short}},
#'   \code{\link{fe_species_bavrn_state}}
#'   Note, that different
#'   yield table assignemnts for the same coding can be defined and coexist. In
#'   future, such tables will be added also for less aggregated species codings.
#'
#' @family yield table functions
#'
#' @name yield_tables_for_species
NULL

#' @rdname yield_tables_for_species
"ytables_bavrn_state_short_var_1"

#' @rdname yield_tables_for_species
"ytables_tum_wwk_short_var_1"

#' @rdname yield_tables_for_species
"ytables_bavrn_state_var_1"



