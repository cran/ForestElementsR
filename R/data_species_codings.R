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




#' Supported Species Codings in the Package **ForestElementsR**
#'
#' @description Data of all supported species codings in the package
#'   \code{ForestElementsR}. Currently supported codings are
#' \describe{
#'   \item{master}{The *master* species coding is the original species coding
#'     used by the package **ForestElementsR**. It contains each species from
#'     the \code{\link{species_master_table}} and no species groups. This
#'     coding corresponds directly to the \code{\link{species_master_table}}.
#'     Its species_id's (see below) are the master table's columns
#'     \code{genus} and \code{species} combined into one character string,
#'     separated by an underscore.}
#'   \item{tum_wwk_short}{The *tum_wwk_short* species coding is one of two
#'     codings in use at the Chair of Forest Growth and Yield Science. It
#'     defines only a small set of single species explicitly (the most important
#'     ones in Central Europe), while all other species are attributed to a few
#'     large container groups.}
#'   \item{tum_wwk_long}{The *tum_wwk_long* species coding is one of two
#'     codings in use at the Chair of Forest Growth and Yield Science. It
#'     defines a larger set of single species than the *tum_wwk_short* coding.
#'     In its original version, this coding contains several species groups, but
#'     most of these groups are ambiguous as they include species which also
#'     have a single coding. These ambiguous groups were not included in this
#'     package.}
#'   \item{bavrn_state}{The *bavrn_state* species coding is the species coding
#'     used by the Bavarian State Forest Service.}
#'   \item{bavrn_state_short}{The *bavrn_state_short* is a coding that combines
#'     the species of *bavrn_state* into groups. These groups are typically used
#'     by the Bavarian State Forest Service in aggregated evaluations.}
#'   \item{ger_nfi_2012}{The *ger_nfi_2012* species coding is the species coding
#'     used by the German National Forest Inventory of 2012
#'     \insertCite{bwi3_methods_2017}{ForestElementsR}}
#' }
#'
#' @format A tibble containing the supported species codings together with the
#'   coding tables (which are tibbles themselves). Its columns are:
#' \describe{
#'   \item{species_coding}{name of the coding}
#'   \item{code_table}{tibble describing the species coding with the columns
#'     \describe{
#'       \item{species_id}{the species code (character)}
#'       \item{species_name_sci}{the scientific species name (for species groups
#'         english terms are used)}
#'       \item{species_name_eng}{English species names}
#'       \item{species_name_ger}{German species names}
#'     }
#'   }
#' }
#'
#' @references
#'   \insertAllCited{}
#'
#'
#' @examples
#' # Get specific coding tables out of the data 'species_codings'
#' fe_species_get_coding_table("master")
#' fe_species_get_coding_table("tum_wwk_short")
#' fe_species_get_coding_table("tum_wwk_long")
#' fe_species_get_coding_table("bavrn_state")
#' fe_species_get_coding_table("bavrn_state_short")
#' fe_species_get_coding_table("ger_nfi_2012")
#'
#' # Check number of species behind each code in a given coding
#' fe_species_get_coding_table("tum_wwk_short") |>
#'   dplyr::group_by(species_id) |>
#'   dplyr::summarise(n = dplyr::n()) |>
#'   dplyr::arrange(as.numeric(species_id)) # just for the look of it
#'
#' fe_species_get_coding_table("bavrn_state_short") |>
#'   dplyr::group_by(species_id) |>
#'   dplyr::summarise(n = dplyr::n()) |>
#'   dplyr::arrange(as.numeric(species_id)) # just for the look of it
#'
"species_codings"
