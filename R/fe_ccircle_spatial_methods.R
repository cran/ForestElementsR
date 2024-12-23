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




#' Plot an fe_ccircle_spatial Object
#'
#' @param x An \code{fe_ccircle_spatial} object
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
#' @param dbh_scale Scaling factor for plotting tree dbh in order to allow
#'   oversized representations. Defaults to 1 (correct scaling)
#'
#' @param show_labels Logical value. If TRUE, labels for species, dbh, R and
#' angle are displayed.
#'
#' @param ... Additional arguments, not used in \code{plot.fe_ccircle_spatial}
#'
#' @return A map (ggplot2) of the plot layout including the trees with
#'   coordinates
#'
#' @export
#'
#' @examples
#'  opt_old <- getOption("fe_spec_lang") # store user's current setting
#'  options(fe_spec_lang = "eng")      # choose Englisch species name display
#'  spruce_pine_ccircle_spatial |> plot()
#'  spruce_pine_ccircle_spatial |> plot(dbh_scale = 4)
#'  spruce_pine_ccircle_spatial |>
#'    plot(
#'      dbh_scale = 4,
#'      tree_filter = species_id == fe_species_tum_wwk_long(30) & dbh_cm > 35
#'    )
#'  options(fe_spec_lang = opt_old)
#'
plot.fe_ccircle_spatial <- function(x,
                                    tree_filter = TRUE,
                                    dbh_scale = 1,
                                    show_labels = FALSE,
                                    ...) {

  plotting_set <- merge(x$tree_positions, x$trees) |>
    dplyr::filter({{ tree_filter }})

  circles <- x$circle_definition
  circles$r <- sqrt(circles$c_area * 10000 / pi)

  buffer_circles <- st_buffer(
    circles,
    sqrt(circles$c_area * 10000 / pi)
  )

  north_segment <- circles |>
    dplyr::filter(.data$r == max(.data$r)) |>
    # create a line with origin geometry and end north until the radius
    # only works in rectangular projected coordinates
    dplyr::mutate(
      geometry = sf::st_sfc(
        sf::st_linestring(
          matrix(
            c(
              sf::st_coordinates(.data$geometry)[, 1],
              sf::st_coordinates(.data$geometry)[, 2],
              sf::st_coordinates(.data$geometry)[, 1],
              sf::st_coordinates(.data$geometry)[, 2] + .data$r
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      )
    ) |>
    # add crs from the tree positions
    sf::st_set_crs(st_crs(plotting_set))

  buffer_dbh <- st_buffer(
    plotting_set,
    plotting_set$dbh_cm * dbh_scale / (2 * 100)
  )


  image <- buffer_dbh |>
    ggplot2::ggplot() +
    # coord_sf(crs = sf::st_crs(31468)) +
    ggplot2::geom_sf(
      color = "black",
      ggplot2::aes(
        fill = format(.data$species_id)
      ),

    ) +
    ggplot2::geom_sf(data = plotting_set, shape = 3) +
    ggplot2::geom_sf(
      color = "black",
      alpha=0,
      data = buffer_circles
    ) +
    ggplot2::labs(title = x$stand_id) +
    ggplot2::scale_fill_discrete("Species")+
    #plot north segment line
    ggplot2::geom_sf(data = north_segment, color = "black")

  # Plot axes labels are not necessary for georeferenced coordinates
  if (is.na(sf::st_crs(x$circle_definition))) {
    image <- image +
      ggplot2::scale_x_continuous("x (m)") +
      ggplot2::scale_y_continuous("y (m)")
  } else {
    image <- image +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()
      )
  }

  # add labels for the trees in the plot


  # plotting_set <- plotting_set |>
  #   dplyr::mutate(dbh_scale_factor = .data$dbh_cm * dbh_scale / 100)


if (show_labels == TRUE) {

    image <- image +
    ggplot2::geom_text(data = plotting_set,
      ggplot2::aes(
        x = sf::st_coordinates(.data$geometry)[, 1],
        y = sf::st_coordinates(.data$geometry)[, 2],
        label = format(.data$species_id)
      ),
      nudge_x =  1.,
      nudge_y =  0.1,
      size = 4
    ) +
    ggplot2::geom_text(data = plotting_set,
      ggplot2::aes(
       x = sf::st_coordinates(.data$geometry)[, 1],
       y = sf::st_coordinates(.data$geometry)[, 2],
       label = .data$dbh_cm
      ),
      ,
      nudge_x = 1.,
      nudge_y = (-0.40),
      size = 3
    )  +
    ggplot2::geom_text(data = plotting_set,
       ggplot2::aes(
       x = sf::st_coordinates(.data$geometry)[, 1],
       y = sf::st_coordinates(.data$geometry)[, 2],
       label = .data$R
                       ),
       nudge_x = 1.,
       nudge_y = (-0.80),
       size = 3
    ) +
    ggplot2::geom_text(data = plotting_set,
     ggplot2::aes(
     x = sf::st_coordinates(.data$geometry)[, 1],
     y = sf::st_coordinates(.data$geometry)[, 2],
     label = .data$angle
    ),
     nudge_x = 1.,
     nudge_y = ( -1.20),
     size = 3
    )

  }

  image
}

