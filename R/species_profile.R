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




#' Species Profile Index After Pretzsch
#'
#' As an extension of the Shannon Index (\code{\link{shannon_index}}), the
#' species profile index by
#' \insertCite{pretzsch_forest_2009;textual}{ForestElementsR}
#' takes into account the vertical structure of a forest stand. For doing so,
#' the function \code{\link{assmann_layers}} is called in the background.
#'
#' Note that this function calculates comparable output only when the same
#' species coding is used for the input parameter \code{species_id}.
#'
#' @param species_id A vector of species codes, each vector element representing
#'   a tree. Preferably, \code{species_id} is defined in one of the species
#'   codings supported by this package, but technically, this is not even
#'   a requirement.
#'
#' @param heights A vector of tree heights, must have the same length as
#'   \code{species_id}
#'
#' @param weights A vector of weights for each tree, default = 1, i.e. all trees
#'   are equally weighted. Must be of length 1 or the same length as
#'   \code{species_id}. Useful if e.g. trees should be weighted by their basal
#'   area.
#'
#' @param n_rep A vector of representation numbers for each tree, typically the
#'   number of trees represented per ha by each tree. Does only make a
#'   difference if it differs among the trees. Default = 1, i.e. all trees have
#'   the same representation number.
#'
#' @param reference_height Reference height for the 100% level of the stand
#'   height profile. Internally passed to \code{\link{assmann_layers}}. If
#'   \code{NULL} (default), the maximum of \code{heights} will be used as the
#'   reference height.
#'
#' @return The Species Profile Index value resulting from the input data
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
#'   species_profile(trees$species_id, trees$height_m)
#'
#'   # Two-species mixed stand
#'   trees <- spruce_beech_1_fe_stand$trees
#'   species_profile(trees$species_id, trees$height_m)
#'
#'   # Selection forest
#'   trees <- selection_forest_1_fe_stand$trees
#'   species_profile(trees$species_id, trees$height_m)
#'
#'   # weigh with basal area (i.e. dbh^2)
#'   species_profile(trees$species_id, trees$height_m, weights = trees$dbh_cm^2)
#'
#'   # weigh with inverse basal area (i.e. 1 / dbh^2)
#'   species_profile(
#'     trees$species_id, trees$height_m, weights = 1 / trees$dbh_cm^2
#'   )
#'
species_profile <- function(species_id,
                            heights,
                            weights = 1,
                            n_rep = 1,
                            reference_height = NULL) {

  length_in <- c(length(weights), length(n_rep))
  if(any(length_in > length(species_id))) {
    stop("No input vector must be longer than species_id.")
  }

  if(length(heights) != length(species_id)) {
    stop("Inputs 'species_id' and 'heights' must have the same length.")
  }

  if (is.null(reference_height)) reference_height <- max(heights)

  work_dat <- tibble::tibble(
    species_id = species_id,
    heights    = heights,
    weights    = weights,
    n_rep      = n_rep
  )

  work_dat <- work_dat |>
    dplyr::mutate(
      assm_layer   = assmann_layers(.data$heights, reference_height),
      total_weight = .data$n_rep * .data$weights
    ) |>
    dplyr::group_by(.data$species_id, .data$assm_layer) |>
    dplyr::summarise(total_weight = sum(.data$total_weight))

  sum_total <- sum(work_dat$total_weight)

  work_dat <- work_dat |>
    dplyr::mutate(
      rel_share     = .data$total_weight / sum_total,
      log_relshare  = ifelse(.data$rel_share > 0, log(.data$rel_share), 0),
      specprof_part = .data$rel_share * .data$log_relshare
    )

  - sum(work_dat$specprof_part)
}


