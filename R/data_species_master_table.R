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




#' List of Supported Species in the Package **ForestElementsR**
#'
#' @description A \code{data.frame} (\code{tibble}) where each row represents a
#'   single species which is - in principle - currently supported by the package
#'   **ForestElementsR**. Each species which is coded (as a single species or a
#'   group of species) in any of the supported \code{\link{species_codings}},
#'   must also be represented in this data frame. No specific species coding,
#'   however, must necessarily use all of the species listed here.
#'
#'   The internal universal species coding used in this package is defined in
#'   the two columns \code{genus} and \code{species_no} (see below).
#'
#'
#' @format A tibble with the following columns are:
#' \describe{
#'   \item{deciduous_conifer}{\code{Character} column with allowed values
#'     \code{conif}, and \code{decid}} for confifer and deciduous species,
#'     respectively
#'   \item{genus}{\code{Character}, the genus of the species in plaintext, but
#'     always lowercase}
#'   \item{species_no}{\code{Character}, a three digit number, always with
#'     leading zeroes, indicating the species inside the genus. A number was
#'     chosen instead of plaintext, because spelling and naming must be
#'     considered somewhat instable on that level. The order of the numbers
#'     reflects (very) roughly the importance of the species inside a given
#'     genus in Central Europe.}
#'   \item{name_sci}{Scientific species names (\code{character})}
#'   \item{name_eng}{Colloquial English species names (\code{character})}
#'   \item{name_ger}{Colloquial German species names (\code{character})}
#' }
#'
"species_master_table"
