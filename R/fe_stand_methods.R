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




#' Summary for an **fe_stand** Object
#'
#' Compute an overview of basic stand level variables for an \code{fe_stand}
#' object
#'
#' The summary calls the function \code{\link{stand_sums_static}} with its
#' default settings. The result is a data.frame (tibble) with species and year
#' wise stand sum values per ha and mean values. Stem numbers, basal areas,
#' quadratic mean diameters and dominant diameters (d 100) are always
#' calculated. Quadratic mean heights, dominant heights (h 100), and wood
#' volumes are only calculated if the heights of all trees are given in
#' \code{object} (i.e. no NA). The summary contains a column
#' \code{species_id}. Depending on the setting of
#' \code{options("fe_spec_lang")}, the species ids will be printed as the
#' species code (settings NULL or "ger"), scientific, English, or German species
#' names (settings "sci", "eng", or "ger", respectively).
#'
#' @param object an \code{fe_stand} object
#'
#' @param ... additional arguments, passed to \code{\link{stand_sums_static}}
#'
#' @return data.frame (tibble) resulting from applying the function
#'   \code{\link{stand_sums_static}} to \code{object}
#'
#' @seealso \link{stand_sums_static}
#'
#' @export
#'
#' @examples
#'
#' # Make a stand data.frame (or nicer, a tibble) that meets the minimum
#' # requirements for setting up a fe_Stand object
#' some_stand <- tibble::tibble(
#'   tree_id = as.character(c(1:100)),
#'   species_id = as_fe_species_tum_wwk_short(
#'     as.character(c(rep(1, 40), rep(5, 60)))
#'   ),
#'   time_yr = rep(2022, 100),
#'   dbh_cm = c(rnorm(40, 40.1, 7.3), rnorm(60, 32.8, 8.4)),
#' )
#'
#' # Make the object
#' some_fe_stand <- fe_stand(
#'   some_stand,
#'   tree_id_col = "tree_id",
#'   species_id_col = "species_id",
#'   time_yr_col = "time_yr",
#'   dbh_cm_col = "dbh_cm",
#'   area_ha = 0.25
#' )
#'
#' # The summary with different language choices
#' options(fe_spec_lang = "code")
#' summary(some_fe_stand)
#' options(fe_spec_lang = "sci")
#' summary(some_fe_stand)
#' options(fe_spec_lang = "eng")
#' summary(some_fe_stand)
#' options(fe_spec_lang = "ger")
#' summary(some_fe_stand)
#'
#'
#' # Use example stands
#' options(fe_spec_lang = "eng")
#' norway_spruce_1_fe_stand |> summary()
#' summary(european_beech_1_fe_stand)
#' options(fe_spec_lang = "sci")
#' summary(selection_forest_1_fe_stand)
#' spruce_beech_1_fe_stand |> summary()
#' options(fe_spec_lang = "code")
#' summary(selection_forest_1_fe_stand)
#' spruce_beech_1_fe_stand |> summary()
#'
summary.fe_stand <- function(object,
                             ...) {
  stand_sums_static(object, ...)
}



#' Plot an **fe_stand** Object
#'
#' Diameter distributions in number of trees per ha, one diagrame by year of
#' survey, tree cohort to be displayed can be filtered.
#'
#' The diagram(s) are made with \code{ggplot2::geom_bar}, the colours for the
#' species, the number and width of the diameter bins correspond to the default
#' settings in ggplot.
#'
#'
#' @param x an \code{fe_stand} object
#'
#' @param tree_filter A \code{data-masking} expression that applies to the
#'   data.frame \code{x$trees}. It must return a logical value, and is defined
#'   in terms of the variables in \code{x$trees}. In this function, it is used
#'   internally in order to define the cohort of trees which is to be evaluated
#'   by this function (within a call to \code{dplyr::filter()}). While many
#'   meaningful filterings are conceivable, distinctions between total stand,
#'   removal stand, and remaining stand are the most probable applications.
#'   Defaults to \code{TRUE}, i.e. all trees are included. See examples.
#'
#' @param ... additional arguments, not used in \code{plot.fe_stand}
#'
#'
#' @return A plot (ggplot2) of the diameter distribution
#'
#' @export
#'
#' @examples
#' # display scientific species names in all examples
#' old_opt <- getOption("fe_spec_lang") # store current user sertting
#' options(fe_spec_lang = "sci") # display scientific names
#'
#' # mixed mountain forest - all trees
#' mm_forest_1_fe_stand_spatial |> plot()
#' # ... remaining trees only
#' mm_forest_1_fe_stand_spatial |> plot(tree_filter = !removal)
#' # ... removal only
#' mm_forest_1_fe_stand_spatial |> plot(tree_filter = removal)
#' # ... all trees with dbh > 30 cm
#' mm_forest_1_fe_stand_spatial |> plot(tree_filter = dbh_cm > 20)
#'
#' # other example stands
#' selection_forest_1_fe_stand |> plot()
#' norway_spruce_1_fe_stand |> plot()
#' spruce_beech_1_fe_stand |> plot()
#'
#' # reset to previous species name settings
#' options(fe_spec_lang = old_opt)
#'
plot.fe_stand <- function(x, tree_filter = TRUE, ...) {
  x$trees |>
    dplyr::filter({{ tree_filter }}) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = .data$dbh_cm, weight = .data$n_rep_ha,
        fill = format(.data$species_id)
      )
    ) +
    ggplot2::scale_x_binned("dbh (cm)") +
    ggplot2::scale_y_continuous("N/ha") +
    ggplot2::scale_fill_discrete("Species") +
    ggplot2::facet_wrap(ggplot2::vars(.data$time_yr))
}




