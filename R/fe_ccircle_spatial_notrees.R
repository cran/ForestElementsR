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




#' Constructor for the **fe_ccircle_spatial_notrees** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_stand}} for creating an
#' object of that class.
#'
#' The class \code{fe_ccircle_spatial_notrees} has been designed for covering
#' a comparably rare case, i.e. an inventory point where no regular trees
#' (trees that are big enough to have a dbh), but possibly small trees are
#' present.
#'
#' @param x An appropriate \code{list} object
#'
#' @param ... Additional arguments required for enabling subclasses of
#'   \code{fe_ccircle_spatial_notrees}
#'
#' @param class A Character string required for enabling subclasses of
#'   \code{fe_ccircle_spatial_notrees}
#'
#' @return An object of class \code{fe_ccircle_spatial_notrees}
#'
#' @export
#'
#' @examples
#' #' # Constructing a minimal fe_ccircle_spatial object from scratch
#' # Use fe_ccircle_spatial() if you are not absolutely sure
#'
#' trees <- NULL
#' tree_positions <- NULL
#'
#' # define a circle definition with three concentric circles
#' circle_def <- data.frame(
#'   dbh_lower = c(0,12,30),
#'   dbh_upper = c(11.9,29.9,999.0),
#'   c_area = c(0.0025, 0.0060, 0.0500)
#' )
#'
#'
#' # generate a NA dummy for small_trees
#'
#'   small_trees <- data.frame(
#'   tree_id = NA_character_,
#'   species_id = NA_character_,
#'   layer_key = NA_real_,
#'   time_yr = NA_real_,
#'   dbh_cm = NA_real_,
#'   age_yr = NA_real_,
#'   height_m = NA_real_
#'   )
#'
#' fe_ccircle_spatial_notrees_candidate <- list(
#'   stand_id = "my_interesting_stand",
#'   small_trees = small_trees,
#'   trees = trees,
#'   circle_definition = circle_def,
#'   tree_positions = tree_positions,
#'   time_yr = 2024
#' )
#'
#' fe_ccircle_notrees_object <-
#' new_fe_ccircle_spatial_notrees(fe_ccircle_spatial_notrees_candidate)
#'
#' # Better validate it
#' fe_ccircle_notrees_object |> validate_fe_ccircle_spatial_notrees()
#'
#'
new_fe_ccircle_spatial_notrees <- function(
    x = list(), ..., class = character()
) {

  stopifnot(is.list(x))
  parent_classes <- class(new_fe_ccircle_spatial(x))
  structure(
    x,
    ...,
    class = c(class, "fe_ccircle_spatial_notrees", parent_classes)
  )

}



#' Check if an Object is an **fe_ccircle_spatial_notrees** object
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the
#'   \code{fe_ccircle_spatial_notrees} class
#'
#' @export
#'
#' @examples
#'   strange <- "xyzabc"
#'   is_fe_ccircle_spatial_notrees(strange)
#'
is_fe_ccircle_spatial_notrees <- function(x) {
  inherits(x, "fe_ccircle_spatial_notrees")
}



#' Validate an **fe_ccircle_spatial_notrees** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor
#' \code{\link{new_fe_ccircle_spatial_notrees}}. Regular users, please construct
#' \code{fe_ccircle_spatial_notrees} objects with
#' \code{\link{fe_ccircle_spatial_notrees}}.
#'
#' @param x An object that is expected to be a correct
#'   \code{fe_ccircle_spatial_notrees} object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the
#'   \code{fe_ccircle_spatial_notrees} object specifications. In case of such
#'   violations, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#'   # Validate the example fe_ccircle_spatial object
#'   spruce_pine_ccircle_spatial_notrees |>
#'     validate_fe_ccircle_spatial_notrees()
#'
#'
validate_fe_ccircle_spatial_notrees <- function(x) {
  # obviously similar but not equal to the validation of an fe_stand_spatial
  # object

  # Check for class attribute
  stopifnot(is_fe_ccircle_spatial(x))

  # Check for presence of the required list elements
  required_names <- c(
    "stand_id",
    "time_yr",         # because no other guaranteed time information
    "trees",
    "tree_positions",
    "small_trees",
    "circle_definition"
  )
  check_rslt <- has_required_names(x, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "element(s)", check_rslt$err_message, "missing in fe_stand object"
      ),
      call. = FALSE
    )
  }

  # Check for correct types or classes of the list elements
  required_types_or_classes <- tibble::tribble(
    ~name, ~required,
    "time_yr", "numeric",
    "trees", "NULL",
    "tree_positions", "NULL",
    "stand_id", "character",
    "small_trees", "data.frame",
    "circle_definition", "data.frame"
  )

  check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("element(s)", check_rslt$err_message),
      call. = FALSE
    )
  }


  # Check for length requirements for list elements with such restrictions
  required_lengths <- tibble::tribble(
    ~name, ~required,
    "stand_id", 1L,
  )

  check_rslt <- has_required_lengths(x, required_lengths)

  if (!check_rslt$rqmt_ok) {
    stop(check_rslt$err_message, call. = FALSE)
  }


  # Check if time information is consistent
  if (any(is.na(x$time_yr))) {
    stop("No NA values allowed in element time_yr", call. = FALSE)
  }

  if (any(duplicated(x$time_yr))) {
    stop("No duplicate values allowed in element time_yr", call. = FALSE)
  }

  if (!is.na(x$small_trees$tree_id[1])) {
    a <- sort(x$time_yr)
    b <- sort(unique(x$small_trees$time_yr))

    if (!identical(a, b)) {
      stop(
        paste0(
          "Element time_yr not consistent with time information in element ",
          "small_trees."
        ),
        call. = FALSE)
    }
  }


  # Check the data.frame circle_definition

  ## Check for presence of the required columns in data.frame trees
  required_names <- c(
    "dbh_lower", "c_area"
  )

  check_rslt <- has_required_names(x$circle_definition, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "column(s)", check_rslt$err_message,
        "missing in `cicle_definition`"
      ),
      call. = FALSE
    )
  }

  # Return the object independently from validation result
  x
}



#' User Friendly Construction of an **fe_ccircle_spatial_notrees** Object from a
#' List of Data Frames
#'
#' \code{fe_ccircle_spatial_notrees()} provides a user-friendly interface for
#' the constructor \code{\link{new_fe_ccircle_spatial_notrees}}. While the
#' constructor does not prevent users from creating malformed
#' \code{new_fe_ccircle_spatial_notrees} objects,
#' \code{new_fe_ccircle_spatial_notrees} does everything to achieve a
#' well-defined object mostly based on an initial list of data.frames that might
#' be, e.g. drawn out of a user's own data base.
#'
#' An object of class \code{fe_ccircle_spatial_notrees} is a child object of
#' \code{\link{fe_ccircle_spatial}} which, however does not contain trees which
#' are big enough to have a dbh. So, in contrast to
#' \code{\link{fe_ccircle_spatial}} objects, where the elements \code{trees},
#' and \code{tree_positions} must be data frames, both are \code{NULL} in this
#' class. The class \code{fe_ccircle_spatial_notrees} has been designed for
#' covering a comparably rare case, i.e. an inventory point where no regular
#' trees (trees that are big enough to have a dbh), but possibly small trees are
#' present. The input object \code{x} to \code{fe_ccircle_spatial} must be a
#' list that follows the same conventions as the input object of
#' \code{\link{fe_ccircle_spatial}}, except that no tree data frame is required.
#' If it does exist, it will be ignored. In addition to the input object of
#' \code{\link{fe_ccircle_spatial}}, there is one additional list element needed
#' which defines a single (calendar) year or a vector of years in order to give
#' the object a time relation. If small tree information is present (i.e. the
#' small tree data frame is not empty), \code{time_yr} must absolutely match
#' with the \code{time_yr} column of this data frame.
#'
#' @param x Named list to be coerced into the goal object.
#'
#' @param time_yr_name name of the element of \code{x} which contains the
#'   required time information, i.e. a single (calendar) year or vector of years
#'   in order to give the object a time relation. If small tree information is
#'   present (i.e. the small tree data frame is not empty), \code{time_yr} must
#'   absolutely match with the \code{time_yr} column of this data frame.
#'
#' @param circle_frame_name Name of the data frame in \code{x} that contains the
#'   definition of the concentric circles. It must contain the lower and the
#'   upper dbh limits, the circle area in Ha, and the slope in degrees. If the
#'   slope is not given it will be set to 0.
#'
#' @param center_coord Name of the sf object that contains the center coordinate
#'   of the circles with coordinate reference system (either Gauss Kruger or
#'   UTM). If it is not provided the center will be c(0,0) by the default
#'
#' @param small_trees_name Name of the object that contains the information
#'   regarding the small trees (generally those trees that do no have any dbh
#'   because of having a height below 1.3 m). This object is still experimental,
#'   so the only requirement for it is that it has to be a data frame without
#'   any further specification. Useful standard information in this data frame
#'   are the tree id, the height and represenation area.
#'
#' @param stand_id arbitrary id of the stand (\code{character}, default:
#'   "my_fe_ccircle_spatial")
#'
#' @return If the user input allows to construct a well-defined
#'   \code{fe_ccircle_spatial_notrees} object, this object will be returned. If
#'   not, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Transform the example data collection mm_forest_1_raw (could e.g. have
#' # been drawn out of a user's data base) into an fe_ccircle_spatial_notrees
#' # object. This means, that in the resulting data frame the trees and their
#' # positions are ignored.
#' notrees_example <- spruce_pine_ccircle_raw |>
#'   fe_ccircle_spatial_notrees()
#'
#' plot(notrees_example)
#'
fe_ccircle_spatial_notrees <- function(
                               x,
                               time_yr_name = "time_yr",
                               circle_frame_name = "circle_definition",
                               center_coord = "center_coordinate",
                               small_trees_name = "small_trees",
                               stand_id = "my_fe_ccircle_spatial_notrees") {

  stopifnot(is.list(x))
  stopifnot(is.data.frame(x[[circle_frame_name]]))

  time_yr <- x[["time_yr"]]

  # In fe_ccircle_spatial objects, these are the most important elements,
  # here, they must be NULL by definition
  trees          <- NULL
  tree_positions <- NULL

  #  Define circle definition, stop if required columns are missing
  circle_definition <- x[[circle_frame_name]]

  check_rslt <- has_required_names(
    circle_definition,
    c("dbh_lower", "c_area")
  )

  if (!check_rslt$rqmt_ok) {
    stop(
      paste0("Column(s) ", check_rslt$err_message, " missing in ",
             "'circle_definition' data frame"),
      call. = FALSE
    )
  }


  #  Check if center coordinate is provided, if not assign (0,0)
  center_coordinate <- x[[center_coord]]

  if (is.null(center_coordinate)) {
    center_coordinate <- data.frame(matrix(nrow = 1, ncol = 0))
    center_coordinate$x <- 0
    center_coordinate$y <- 0

    center_coordinate <- center_coordinate |>
      sf::st_as_sf(coords = c("x", "y"))
  }

  # add center coordinate geometry to circle definition
  circle_definition <- merge(circle_definition, center_coordinate)
  sf::st_geometry(circle_definition) <-  circle_definition$geometry

  # small trees data frame with at least three columns
  small_trees <- x[[small_trees_name]]

  # Most important: call the constructor
  fe_ccircle_spatial_notrees_candidate <- new_fe_ccircle_spatial_notrees(
    list(
      stand_id          = trimws(stand_id, which = "both"),
      time_yr           = time_yr,
      circle_definition = circle_definition,
      tree_positions    = tree_positions,
      trees             = trees,
      small_trees       = small_trees
    )
  )

  # Validate the object
  fe_ccircle_spatial_notrees_candidate <-
    validate_fe_ccircle_spatial_notrees(fe_ccircle_spatial_notrees_candidate)


  # Actually no candidate anymore if it made it that far
  fe_ccircle_spatial_notrees_candidate
}
