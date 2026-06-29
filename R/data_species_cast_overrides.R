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




#' Declarative Registry of Species Coding Cast Overrides
#'
#' @description A small table of deliberate target codes for casts that would
#'   otherwise be forward-ambiguous. Some source \emph{group} codes have species
#'   that straddle several groups in a goal coding, so there is no single
#'   matching target node. For such a source code this registry declares the
#'   chosen target; the cast then resolves to it (lossily, with a message)
#'   instead of raising an ambiguity error. Currently it holds three cases, all
#'   into \code{tum_wwk_short}:
#'   \itemize{
#'     \item \code{ger_nfi_2012} code \code{"290"} (other broadleaves of low
#'       longevity, whose species mostly map to \code{"8"} but include the horse
#'       chestnut, which alone would be \code{"9"}) resolved to \code{"8"};
#'     \item \code{bavrn_state} code \code{"70"} (the oak group, whose native
#'       oaks map to \code{"6"} but whose red oak would be \code{"8"}) resolved
#'       to \code{"6"};
#'     \item \code{bavrn_state} code \code{"80"} (other broadleaves, straddling
#'       the hardwood \code{"8"} and softwood \code{"9"} groups) resolved to
#'       \code{"8"}.
#'   }
#'
#'   The object is built from the editable CSV
#'   \code{data-raw/codings/cast_overrides.csv} via
#'   \code{\link{cast_overrides_from_csv}} (CSV-driven, like the codings) and is
#'   consulted internally by the casting machinery.
#'
#' @format A tibble with one row per override and the columns
#' \describe{
#'   \item{coding_from}{name of the source coding}
#'   \item{coding_to}{name of the goal coding}
#'   \item{species_id_from}{the forward-ambiguous source code}
#'   \item{species_id_to}{the deliberately chosen target code in the goal coding}
#' }
#'
#' @seealso \code{\link{cast_overrides_from_csv}}, \code{\link{species_codings}}
#'
#' @examples
#' species_cast_overrides
#'
"species_cast_overrides"
