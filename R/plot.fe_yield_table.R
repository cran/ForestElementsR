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




#' Plot an fe_yield_table Object
#'
#' @param x An object of class \code{\link{fe_yield_table}}
#'
#' @param variable Character, name of the variable to be plotted, default is
#'   \code{NA}, which plots the variable listed first in the fe_yield_table
#'   object's \code{site_index_variable slot}.
#'
#' @param ... Other parameters, not used
#'
#' @return An object of class ggplot
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   fe_ytable_pine_wiedemann_moderate_1943 |> plot()
#'   fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "ba_m2_ha")
#'   fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "mai_m3_ha_yr")
#'
#'   # Modify plot post hoc ...
#'   fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "n_ha")
#'   # ... better to read on log scale
#'   fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "n_ha") +
#'     ggplot2::scale_y_log10()
#'
plot.fe_yield_table <- function(x, variable = NA, ...) {

  if(is.na(variable)) variable <- x$site_index_variable[1]

  if (!(variable %in% names(x$values))) {
    stop(
      paste0("Variable '", variable, "' not contained in yield table ",
             x$name_international),
      call. = FALSE
    )
  }

  # Prepare data for plotting
  data <- x$values[[variable]] |>
    tibble::as_tibble() |>
    dplyr::mutate(age = x$age_coverage) |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("si_"), names_to = "si", values_to = "y"
    ) |>
    dplyr::filter(!is.na(.data$y))


  # The actual plot (very basic in order to allow for user modifications)
  data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = .data$age, y = .data$y, group = .data$si,
                   linetype = .data$si)
    ) +
    ggplot2::labs(title = x$name_international, x = "age (yrs)", y = variable)
}


