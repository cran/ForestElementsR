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




#' Example Stands
#'
#'
#' @description
#' **ForestelementsR** comes with five example stands that exist each in a 'raw'
#' format that could have been taken from a user data base, and one of the
#' package's specific stand object classes (**fe_stand**, **fe_stand_spatial**,
#' ,**fe_ccircle_spatial**). Four of the example stands correspond to the
#' **fe_stand** class, one to the
#' **fe_stand_spatial**  and one to **fe_ccircle_spatial**class.
#' The **fe_stand**, **fe_stand_spatial** and  **fe_ccircle_spatial** objects
#' have been constructed from the raw data using the functions
#' \code{\link{fe_stand}},  \code{\link{fe_stand_spatial}} and
#' \code{\link{fe_ccircle_spatial}} (see the examples section of the functions'
#' documentation).
#'
#' \describe{
#'   \item{**fe_stand** compatible examples:}{
#'     The first stand is a 53 years-old Norway spruce stand, the second one
#'     a 79 year old European beech stand, the third an even-aged mixed stand of
#'     Norway spruce and European beech (75 and 88 years, respectively). The
#'     fourth stand is a selection forest comprising the species Norway spruce,
#'     silver fir, European beech, and sycamore. All these stands have an area
#'     of 0.49 ha. The raw data (see name suffix) are data.frames (tibbles)
#'     comprising the following columns
#'     \describe{
#'       \item{\code{stand}}{Name of the stand}
#'       \item{\code{species}}{Integer numbers following the
#'       \code{TUM_WWK_short} convention}
#'       \item{\code{no}}{Tree id}
#'       \item{\code{age}}{Tree age (i.e. stand age, in these examples) in
#'         years; note that the selection forest data do not provide age
#'         information, as this is usually not available in practice and mostly
#'         not meaningful for this forest type}
#'       \item{\code{d}}{Breast height (1.3 m) stem diameter in cm}
#'       \item{\code{h}}{Tree height in m}
#'       \item{\code{hcb}}{Crown base height in m}
#'       \item{\code{crad}}{Crown radius in m}
#'     }
#'     The fe_stand objects (see name suffix) resulted from converting the
#'     raw data with \code{\link{fe_stand}}
#'   }
#'   \item{**fe_stand_spatial** compatible example:}{
#'     **mm_forest_1_raw** represents raw data from a research plot in a mixed
#'     mountain forest that has been surveyed several times.
#'     **mm_forest_1_fe_stand_spatial** is the representation of this plot as an
#'     object of class \code{\link{fe_stand_spatial}}
#'   }
#'    \item{**fe_ccircle_spatial** compatible examples}{
#'      **spruce_pine_ccircle_raw** represents raw data that could come from a
#'      typical inventory plot of the Bavarian State Forest, while
#'      **spruce_pine_ccircle_spatial** is an object of class
#'      \code{\link{fe_ccircle_spatial}}.
#'   }
#' }
#'
#' @name example_data
NULL

#' @rdname example_data
"norway_spruce_1_raw"
#' @rdname example_data
"norway_spruce_1_fe_stand"

#' @rdname example_data
"european_beech_1_raw"
#' @rdname example_data
"european_beech_1_fe_stand"


#' @rdname example_data
"spruce_beech_1_raw"
#' @rdname example_data
"spruce_beech_1_fe_stand"


#' @rdname example_data
"selection_forest_1_raw"
#' @rdname example_data
"selection_forest_1_fe_stand"

#' @rdname example_data
"mm_forest_1_raw"
#' @rdname example_data
"mm_forest_1_fe_stand_spatial"


#' @rdname example_data
"spruce_pine_ccircle_raw"
#' @rdname example_data
"spruce_pine_ccircle_spatial"
#' @rdname example_data
"spruce_pine_ccircle_spatial_notrees"





