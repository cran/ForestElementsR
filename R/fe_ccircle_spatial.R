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




#' Constructor for the **fe_ccircle_spatial** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_stand}} for creating an
#' object of that class.
#'
#' @param x An appropriate \code{list} object
#'
#' @param ... Additional arguments required for enabling subclasses of
#'   \code{fe_ccircle_spatial}
#'
#' @param class A Character string required for enabling subclasses of
#'   \code{fe_ccircle_spatial}
#'
#' @return An object of class \code{fe_ccircle_spatial}
#'
#' @export
#'
#' @examples
#'
#' #' # Constructing a minimal fe_ccircle_spatial object from scratch
#' # Use fe_ccircle_spatial() if you are not absolutely sure
#'
#' trees <- data.frame(
#'   tree_id = as.character(c(1:10)),
#'   species_id = as_fe_species_tum_wwk_short(rep("5", 10)),
#'   layer_key = 1,
#'   time_yr = 2024,
#'   dbh_cm = rnorm(10, 50, 8),
#'   age_yr = NA_real_,
#'   height_m = NA_real_,
#'   crown_base_height_m = NA_real_,
#'   crown_radius_m = NA_real_,
#'   removal = FALSE,
#'   ingrowth = FALSE,
#'   n_rep_ha = 1 / 0.75
#' )
#'
#' # define a circle definition with three concentric circles
#' circle_def <- data.frame(
#'   dbh_lower = c(0,12,30),
#'   dbh_upper = c(11.9,29.9,999.0),
#'   c_area = c(0.0025, 0.0060, 0.0500)
#' )
#'
#' # generate tree positions in polar coordinates for the 10 trees
#'
#' tree_positions <- data.frame(
#'  tree_id = as.character(c(1:10)),
#'  R = runif(10, 1, 12),
#'  angle = runif(10, 0, 360)) |>
#'  dplyr::mutate(
#'  #convert the polar coordinates to cartesian and into an sf object
#'  x_pos = R*sin(angle*pi / 180),
#'  y_pos = R*cos(angle*pi / 180)
#'  ) |> sf::st_as_sf(coords = c("x_pos", "y_pos"))
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
#' fe_ccircle_spatial_candidate <- list(
#'   stand_id = "my_interesting_stand",
#'   small_trees = small_trees,
#'   trees = trees,
#'   circle_definition = circle_def,
#'   tree_positions = tree_positions,
#'   time_yr = 2024
#' )
#'
#' fe_ccircle_object <- new_fe_ccircle_spatial(fe_ccircle_spatial_candidate)
#'
#' # Better validate it
#' fe_ccircle_object |> validate_fe_ccircle_spatial()
#'
#'
new_fe_ccircle_spatial <- function(x = list(), ..., class = character()) {

  stopifnot(is.list(x))
  parent_classes <- class(new_fe_stand_spatial(x))
  structure(
    x,
    ...,
    class = c(class, "fe_ccircle_spatial", parent_classes)
  )

}



#' Check if an Object is an **fe_ccircle_spatial**
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_ccircle_spatial}
#'   class
#'
#' @export
#'
#' @examples
#' strange <- "xyzabc"
#' is_fe_ccircle_spatial(strange)
#'
is_fe_ccircle_spatial <- function(x) {
  inherits(x, "fe_ccircle_spatial")
}



#' Validate an **fe_ccircle_spatial** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{new_fe_ccircle_spatial}}.
#' Regular users, please construct \code{fe_ccircle_spatial} objects with
#' \code{\link{fe_ccircle_spatial}}.
#'
#' @param x An object that is expected to be a correct \code{fe_ccircle_spatial}
#'   object
#'
#' @param method Character string that specifies whether tree_positions is
#'   allowed to contain less tree_ids than trees (i.e. in this case, not all
#'   trees have coordinates). Possible choices are "strict" (default) and
#'   "flexible". If method == "flexible", a warning is issued if not all trees
#'   have coordinates. If method == "strict", the validation terminates with an
#'   error.
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the \code{fe_ccircle_spatial}
#'   object specifications. In case of such violations, the function will
#'   terminate with an error.
#'
#' @export
#'
#' @examples
#'   # Validate the example fe_ccircle_spatial object
#'   spruce_pine_ccircle_spatial |>
#'     validate_fe_ccircle_spatial(method = "flexible")
#'
validate_fe_ccircle_spatial <- function(x, method = c("strict", "flexible")) {

  method = match.arg(method)

  # obviously similar but not equal to the validation of an fe_stand_spatial
  # object

  # Check for class attribute
  stopifnot(is_fe_ccircle_spatial(x))

  # Check for presence of the required list elements
  required_names <- c(
    "stand_id", "trees", "tree_positions",
    "small_trees", "circle_definition", "time_yr"
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
    "trees", "data.frame",
    "tree_positions", "sf",
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


  # Check the data.frame trees

  ## Check for presence of the required columns in data.frame trees
  required_names <- c(
    "tree_id", "species_id", "layer_key", "time_yr", "age_yr", "dbh_cm",
    "height_m", "crown_base_height_m", "crown_radius_m", "removal", "ingrowth",
    "n_rep_ha"
  )
  check_rslt <- has_required_names(x$trees, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "column(s)", check_rslt$err_message, "missing in `trees` data.frame"
      ),
      call. = FALSE
    )
  }


  ## Check for required column types (empty columns (if allowed) must have the
  ## correct NA type), species_id is not included here, will be checked below
  required_types <- tibble::tribble(
    ~name,                    ~required,
    "tree_id",                "character",
    "layer_key",              "numeric",
    "time_yr",                "numeric",
    "age_yr",                 "numeric",
    "dbh_cm",                 "numeric",
    "height_m",               "numeric",
    "crown_base_height_m",    "numeric",
    "crown_radius_m",         "numeric",
    "removal",                "logical",
    "ingrowth",               "logical",
    "n_rep_ha",               "numeric"
  )
  check_rslt <- has_required_types_or_classes(x$trees, required_types)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "incorrect column type(s) in `trees` data.frame:",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }


  ## Check species_id
  ### - Coding supported?
  spcs_coding <- fe_species_get_coding(x$trees$species_id)
  if (length(spcs_coding) == 0) spcs_coding <- ""

  if (!(spcs_coding %in% ForestElementsR::species_codings$species_coding)) {
    stop(
      paste(
        "data.frame `trees`:",
        "unsupported species coding of column `species_id`"
      ),
      call. = FALSE
    )
  }


  ### - All species codes ok?
  check_rslt <- all_codes_allowed(x$trees$species_id, spcs_coding)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("data.frame `trees`:", check_rslt$err_message),
      call. = FALSE
    )
  }


  ## Check for missing values in columns which must be complete
  required_complete <- c(
    "tree_id", "species_id", "layer_key", "time_yr", "dbh_cm", "removal",
    "ingrowth", "n_rep_ha"
  )

  check_rslt <- has_no_missing_values(x$trees, required_complete)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "data.frame `trees`:",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }


  ## Check for columns that must be distinct or distinct in combination
  required_unique <- list(
    c("tree_id", "time_yr")
  )

  check_rslt <- is_distinct(x$trees, required_unique)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "data.frame `trees`:",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }

  # Check layer_key for allowed values
  check_rslt <- any(
    x$trees$layer_key |> purrr::map_lgl(.f = ~ !(.x %in% c(1:4)))
  )

  if (check_rslt) {
    stop(
      "data.frame `trees`: column `layer_key` has values other than 1, 2, 3, 4",
      call. = FALSE
    )
  }


  # Check the data.frame (sf) tree_positions

  ## Check for presence of the required columns in data.frame tree_positions
  required_names <- c(
    "tree_id",
    "geometry"
  )
  check_rslt <- has_required_names(x$tree_positions, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "column(s)", check_rslt$err_message,
        "missing in `tree_positions` data.frame (sf)"
      ),
      call. = FALSE
    )
  }


  ## Check for required column types (empty columns (if allowed) must have the
  ## correct NA type), species_id is not included here, will be checked below
  required_types <- tibble::tribble(
    ~name,                    ~required,
    "tree_id",                "character",
    "geometry",               "sfc"
  )

  check_rslt <- has_required_types_or_classes(x$tree_positions, required_types)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "incorrect column type(s) in `tree_positions` data.frame:",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }


  ## Check for columns that must be distinct or distinct in combination
  required_unique <- list(
    "tree_id"
  )

  check_rslt <- is_distinct(x$tree_positions, required_unique)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "data.frame `tree_positions`:",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }


  ## Check geometry column for POINT property in each line
  ## Missing values in geometry column are already blocked by sf
  geomtry_types <- sf::st_geometry_type(x$tree_positions) |> as.character()
  check_rslt <- all(geomtry_types == "POINT")

  if (!check_rslt) {
    stop(
      "geometry type in `tree_positions` must be exclusively `POINT`",
      call. = FALSE
    )
  }


  ## tree_id in trees and tree_positions must exactly match
  ### Conversion into tibbles to preserve data.frame like structure after
  ### single-column selection
  trees_ids <- dplyr::select(
    tibble::as_tibble(x$trees), "tree_id"
  )
  tree_pos_ids <- dplyr::select(
    tibble::as_tibble(x$tree_positions), "tree_id"
  )

  ### 1 - More trees in trees than in tree_positions?
  check_rslt <- 0 == nrow(
    dplyr::anti_join(trees_ids, tree_pos_ids, by = "tree_id")
  )

  if (!check_rslt) {

    if (method == "flexible"){

      warning(
        "some trees do not have coordinates",
        call. = FALSE

      )
    } else {
      stop(
        "more tree_ids in `trees` than in `tree_positions`",
        call. = FALSE

      )
    }

  }


  ### 2 - More trees in tree_positions than in trees?

  check_rslt <- 0 == nrow(
    dplyr::anti_join(tree_pos_ids, trees_ids, by = "tree_id")
  )

  if (!check_rslt) {
    stop(
      "more tree_ids in `tree_positions` than in `trees`",
      call. = FALSE
    )
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

  # Check if trees are withing their circle limits

  if (!is.null(unique(x$circle_definition$slope))) {

    slope_rad = unique(x$circle_definition$slope) *(180/pi)

  } else {

    slope_rad = 0

  }

  check_rslt <- merge(x$tree_positions, x$trees) |>
    dplyr::mutate(
      circle_R = round(sqrt((1 / (.data$n_rep_ha*cos(slope_rad)^2)) / pi) * 100,
                                                                    digits = 2),
      rqmt_ok = .data$circle_R > .data$R
    )

  # check_rslt$rqmt_ok[1] <- FALSE

  if (!all(check_rslt$rqmt_ok == TRUE)) {
    trees_out <- check_rslt |>
      dplyr::filter(.data$rqmt_ok == FALSE) |>
      tibble::as_tibble() |>
      purrr::pluck("tree_id")

    warning(
      paste(
        "trees", trees_out,
        "are out of circle boundaries"
      ),
      call. = FALSE
    )
  }

  # Check if time information is consistent
  if (any(is.na(x$time_yr))) {
    stop("No NA values allowed in element time_yr", call. = FALSE)
  }

  if (any(duplicated(x$time_yr))) {
    stop("No duplicate values allowed in element time_yr", call. = FALSE)
  }

  a <- sort(x$time_yr)
  b <- sort(unique(x$trees$time_yr))

  if (!identical(a, b)) {
    stop(
      paste0(
        "Element time_yr not consistent with time information in element ",
        "small_trees."
      ),
      call. = FALSE)
  }


  if (!is.na(x$small_trees$tree_id[1])) {

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

  # Return the object independently from validation result
  x
}



#'User Friendly Construction of an **fe_ccircle_spatial** Object from a List of
#'Data Frames
#'
#'\code{fe_ccircle_spatial()} provides a user-friendly interface for the
#'constructor \code{\link{new_fe_ccircle_spatial}}. While the constructor does
#'not prevent users from creating malformed \code{fe_ccircle_spatial} objects,
#'\code{fe_ccircle_spatial} does everything to achieve a well-defined object
#'mostly based on an initial list of data.frames that might be, e.g. drawn out
#'of a user's own data base.
#'
#'An object of class \code{fe_ccircle_spatial} is a child object of
#'\code{fe_stand_spatial} which, however, contains information about the
#'horizontal positions of the trees in a concentric circle representation
#'scheme. All spatial information and its processing is based on the R-package
#'[sf](https://CRAN.R-project.org/package=sf).
#'
#'The input object \code{x} to \code{fe_ccircle_spatial} must be a list that
#'comprises two and an optional third data frame(s): \itemize{ \item a data
#'frame containing the single tree information (same requirements as for
#'\code{\link{fe_stand}}). This data frame must contain a a minimum set of
#'columns (tree id, species id, time variable, diameter at breast height). These
#'columns must not contain missing values. Other columns (containing tree
#'height, height to crown base, crown radius, tree age) are optional for the
#'user to provide. If provided, they may contain missing values. If not
#'provided, these columns will only contain missing values in the
#'\code{fe_ccircle_spatial} object. The columns about the trees' removal and
#'ingrowth status are also optional, but if provided, they must *not* contain
#'missing values. If not provided, both columns will be filled with \code{FALSE}
#'in the resulting \code{fe_ccircle_spatial} object.
#'\code{fe_ccircle_spatial} will automatically add a column \code{n_rep_ha}
#'which contains for each tree the number of trees it represents per ha. This
#'may seem redundant if looking at \code{fe_ccircle} objects alone, but it
#'allows a broad range of evaluation functions to be applied to different
#'objects containing trees. In addition to the input object of
#' \code{\link{fe_ccircle_spatial}}, there is one additional list element needed
#' which defines a single (calendar) year or a vector of years in order to give
#' the object a time relation. If small tree information is present (i.e. the
#' small tree data frame is not empty), \code{time_yr} must absolutely match
#' with the \code{time_yr} column of this data frame.
#'
#'\item a data frame that contains information about the tree positions. This is
#'not part of the first data frame, because the latter could contain several
#'observations (at different times) of the same tree, which would lead to
#'redundant coordinate representation. This data frame must contain a column
#'with tree id's, and the x and y coordinates of the stem center points.
#'\code{NA} values are not allowed in this data frame.}
#'
#'
#'@param x named list of two data frames to be coerced into the goal object. One
#'  data frame must contain the single tree data; it has the same requirements
#'  as for the input data frame \code{x} of the function \code{\link{fe_stand}}
#'  or  \code{\link{fe_stand_spatial}}. Another data frame must contain the tree
#'  positions.
#'
#'@param method name of the method for dealing with trees without positions. If
#'  method = "flexible" the validator issues a warning. By default (method =
#'  "strict"), all trees must have defined positions and the validator issues an
#'  error. Trees without positions are typically trees below a certain dbh
#'  threshold. Note that this check applies only to trees that actually have a
#'  dbh, not to trees in the \code{small_trees} slot of an
#'  \code{fe_ccircle_spatial object}.
#'
#'@param tree_frame_name name of the data frame in \code{x} that contains the
#'  single tree data (default: "trees")
#'
#'@param tree_pos_frame_name name of the data frame in \code{x} that contains
#'  the tree positions (default: "tree_positions"), it must contain the tree_id
#'  the radius and the angle in polar coordinates. The angle must be
#'  defined in degrees with the Y-Axis as North (anti-clockwise) and
#'  the radius in meters.
#'
#'@param circle_frame_name name of the data frame in \code{x} that contains the
#'  definition of the concentric circles. It must contain the lower and the
#'  upper dbh limits, the circle area in Ha, and the slope in degrees. If the
#'  slope is not given it will be set to 0.
#'
#'@param center_coord name of the sf object that contains the center coordinate
#'  of the circles with coordinate reference system (either Gauss Kruger or
#'  UTM). If it is not provided the center will be c(0,0) by the default
#'
#'@param small_trees_name name of the object that contains the information
#'  regarding the small trees (generally those trees that do no have any dbh
#'  because of having a height below 1.3 m). This object is still experimental,
#'  so the only requirement for it is that it has to be a data frame with at
#'  least one row with column names. Useful standard information in this data
#'  frame are the tree id, the height and representation area.
#'
#'@param tree_id_col name of the column in the trees and tree_positions data
#'  frames  which contains the tree id's (\code{character}, required, must not
#'  contain missing values)
#'
#'@param species_id_col name of the column in trees data frame which contains
#'  the species id's. Must be an object of one  of the \code{fe_species} classes
#'  supported by this package. This column is required, must not contain missing
#'  values.
#'
#'@param time_yr_col name of the column in the trees data frame which provides
#'  time information in years (\code{character}, required, must not contain
#'  missing values)
#'
#'@param dbh_cm_col name of the column in the trees data frame which contains
#'  the dbh in cm (\code{character}, required, must not contain missing values)
#'
#'@param radius_col name of the column in the tree positions data frame that
#'  contains the distance to the center of the plot (\code{character}).
#'
#'@param angle_col name of the column in the tree positions data frame that
#'  contains the angle coordinate of the tree in polar coordinates. The angle
#'  must be measured respect to the x axis and anticlockwise (\code{character}).
#'
#'@param stand_id arbitrary id of the stand (\code{character}, default:
#'  "my_fe_ccircle_spatial")
#'
#'@param layer_key_col name of the column in \code{x} that contains codes for
#'   the stand layer a given tree belongs to. These codes are whole numbers.
#'   The following values are allowed: 1 - Main stand, 2 - Understorey,
#'   3 - Pregeneration (ger: "Vorausverjuengung"), 4 - Remnant trees, hold over
#'   trees, veteran trees (ger: "Nachhiebsreste", "Ueberhaelter", "Altbaeume").
#'   Must not contain missing values if provided. If not provided, it will be
#'   set to 1 (main stand) for every tree.
#'
#'@param time_yr_name name of the element of \code{x} which contains the
#'   required time information, i.e. a single (calendar) year or vector of years
#'   in order to give the object a time relation. If small tree information is
#'   present (i.e. the small tree data frame is not empty), \code{time_yr} must
#'   absolutely match with the \code{time_yr} column of this data frame.
#'
#'@param age_yr_col name of the column in the trees data frame which provides
#'  the tree ages in years (\code{character}, optional, may contain missing
#'  values)
#'
#'@param height_m_col name of the column in the trees data frame which provides
#'  the tree heights in m (\code{character}, optional, may contain missing
#'  values)
#'
#'@param crown_base_height_m_col name of the column in the trees data frame
#'  which provides the crown base heights in m (\code{character}, optional, may
#'  contain missing values)
#'
#'@param crown_radius_m_col name of the column in the trees data frame which
#'  provides the crown radii in m (\code{character}, optional, may contain
#'  missing values)
#'
#'@param removal_col name of the column in the trees data frame which provides
#'  the tree's removal status. If \code{TRUE}, this indicates the tree was
#'  removed or dead at the time indicated in \code{age_yr_col}
#'  (\code{character}, optional, must not contain missing values if provided).
#'  If not provided, the removal status will be \code{FALSE} for all trees in
#'  the resulting \code{fe_ccircle_spatial} object.
#'
#'@param ingrowth_col name of the column in the trees data frame which provides
#'  the tree's ingrowth status. If \code{TRUE}, this indicates a tree that grew
#'  newly in at the time indicated in \code{age_yr_col} (\code{character},
#'  optional, must not contain missing values if provided). If not provided, the
#'  ingrowth status will be \code{FALSE} for all trees in the resulting
#'  \code{fe_ccircle_spatial} object.
#'
#'@param verbose name of the column in the trees data frame which provides the
#'  tree's ingrowth status. If \code{TRUE}, this indicates a tree that grew
#'  newly in at the time indicated in \code{age_yr_col} (\code{character},
#'  optional, must not contain missing values if provided). If not provided, the
#'  ingrowth status will be \code{FALSE} for all trees in the resulting
#'  \code{fe_ccircle_spatial} object.
#'
#'@param n_rep_ha_col name of the column in the trees data frame which provides
#'  each tree's representation number per ha. n_rep_ha will be always
#'  recalculated based on the representation area of each of the concentric
#'  circles.
#'
#'@return If the user input allows to construct a well-defined
#'  \code{fe_ccircle_spatial} object, this object will be returned. If not, the
#'  function will terminate with an error.
#'
#'@export
#'
#' @examples
#'
#' # Transform the example data collection mm_forest_1_raw (could e.g. have
#' # been drawn out of a user's data base) into an fe_ccircle_spatial object
#'
#' spruce_pine_ccircle_sp <- spruce_pine_ccircle_raw |>
#'   fe_ccircle_spatial(
#'     method = "flexible",
#'     tree_id_col = "tree_id",
#'     species_id_col = "species_id",
#'     time_yr_col = "time_yr",
#'     dbh_cm_col = "dbh_cm",
#'     radius_col = "R",
#'     angle_col = "angle",
#'     stand_id = mm_forest_1_raw$stand_id,
#'     height_m_col = "height_m",
#'     removal_col = "removal",
#'     time_yr_name = "time_yr"
#'   )
#' # Show a little summary, display scientific species names
#' options(fe_spec_lang = "sci")
#' spruce_pine_ccircle_sp |> summary()
#'
fe_ccircle_spatial <- function(x,
                               method = c("strict", "flexible"),
                               tree_frame_name = "trees",
                               tree_pos_frame_name = "tree_positions",
                               circle_frame_name = "circle_definition",
                               center_coord = "center_coordinate",
                               small_trees_name = "small_trees",
                               time_yr_name = "time_yr",
                               tree_id_col,
                               species_id_col,
                               time_yr_col,
                               dbh_cm_col,
                               radius_col,
                               angle_col,
                               stand_id = "my_fe_ccircle_spatial",
                               layer_key_col = NA,
                               age_yr_col = NA,
                               height_m_col = NA,
                               crown_base_height_m_col = NA,
                               crown_radius_m_col = NA,
                               removal_col = NA,
                               ingrowth_col = NA,
                               n_rep_ha_col = NA,
                               verbose = TRUE) {

  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(
    x[c(
      tree_frame_name, tree_pos_frame_name, circle_frame_name
    )],
    is.data.frame
  )))

  time_yr <- x[["time_yr"]]


  method = match.arg(method)


  # <--- Start check for missing required arguments
  required_args <- c(
    "tree_id_col", "species_id_col", "time_yr_col", "dbh_cm_col", "radius_col",
    "angle_col"
  )

  passed_args <- names(as.list(match.call())[-1])
  missing_required <- setdiff(required_args, passed_args)

  if (length(missing_required) > 0) {
    stop(
      paste(
        "Required arguments",
        paste(missing_required, collapse = ", "),
        "missing"
      ),
      call. = FALSE
    )
  }
  # ---> End check for missing required arguments

  # Make the core data.frame trees of an fe_ccircle_spatial object
  # Remember: After assigning the class (below), the column n_rep_ha will be
  # added to this data.frame
  trees <- make_trees_dataframe(
    x[[tree_frame_name]],
    tree_id_col,
    species_id_col,
    time_yr_col,
    dbh_cm_col,
    layer_key_col,
    age_yr_col,
    height_m_col,
    crown_base_height_m_col,
    crown_radius_m_col,
    removal_col,
    ingrowth_col,
    n_rep_ha_col
  )


  #  Define circle definition, stop if required columns are missing
  circle_definition <- x[[circle_frame_name]]

  check_rslt <- has_required_names(
    circle_definition,
    c("dbh_lower", "c_area")
  )


  #  Check if center coordinate is provided, if not assign (0,0)
  center_coordinate <- x[[center_coord]]

  if (is.null(center_coordinate)) {

    center_coordinate <- data.frame(matrix(nrow= 1, ncol = 0))
    center_coordinate$x <- 0
    center_coordinate$y <- 0

    center_coordinate <- center_coordinate |>
      sf::st_as_sf(coords = c("x", "y"))



  }

  # if the column geometry is not present in circle definition merge it with
  # center_coordinate
  if (!"geometry" %in% names(circle_definition)) {
    circle_definition <- merge(circle_definition, center_coordinate)
    sf::st_geometry(circle_definition) <-  circle_definition$geometry
  }


  # sum the centers of the circles to the center_coordinate (default are 0,0)
  circle_definition <- circle_definition |>
    dplyr::mutate(geometry = center_coordinate$geometry + .data$geometry) |>
    sf::st_as_sf() |>
    sf::st_set_crs(sf::st_crs(center_coordinate))


  # transform polar coordinates into cartesian (degrees must be converted
  # to radians)
  tree_positions <- x[[tree_pos_frame_name]]

  # filter out trees without coordinates, this will be later fall into a
  # warning or an error when validated depending on chosen method
  tree_positions <- tree_positions |>
    dplyr::filter(!is.na(.data$R), !is.na(.data$R))


  check_rslt <-
    has_required_names(tree_positions, c(tree_id_col, radius_col, angle_col))


  tree_positions <- tree_positions |>
    dplyr::mutate(
      x_m_col = .data$R * sin(.data$angle  * pi / 180),
      y_m_col = .data$R * cos(.data$angle  * pi / 180)
    )

  # Make the tree_positions sfc, stop if required columns are missing
  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "column(s)", check_rslt$err_message,
        "missing in `tree_positions` data.frame"
      ),
      call. = FALSE
    )
  }

  tree_positions <- tree_positions |>
    dplyr::rename(tree_id = tidyselect::all_of(tree_id_col)) |>
    dplyr::mutate(tree_id = as.character(.data$tree_id)) |>
    sf::st_as_sf(coords = c("x_m_col", "y_m_col"))


  # add center coordinates (default are 0,0) to tree coordinates
  tree_positions <- tree_positions |>
    dplyr::mutate(geometry = center_coordinate$geometry + .data$geometry) |>
    sf::st_set_crs(sf::st_crs(center_coordinate))





  # small trees data frame with at least three columns
  small_trees <- x[[small_trees_name]]


  # Most important: call the constructor
  fe_ccircle_spatial_candidate <- new_fe_ccircle_spatial(
    list(
      stand_id          = trimws(stand_id, which = "both"),
      time_yr           = time_yr,
      circle_definition = circle_definition,
      tree_positions    = tree_positions,
      trees             = trees,
      small_trees       = small_trees
    )
  )

  # calculate n_rep_ha in the data.frame trees (done here, because the
  # function n_rep_ha is an S3 generic)
  fe_ccircle_spatial_candidate$trees <- fe_ccircle_spatial_candidate$trees |>
    dplyr::mutate(n_rep_ha = n_rep_ha(fe_ccircle_spatial_candidate))


  # Validate the object
  fe_ccircle_spatial_candidate <-
    validate_fe_ccircle_spatial(fe_ccircle_spatial_candidate, method)


  # Optional checks for plausible orders of magnitude
  if (verbose) {
    check_trees_orders_of_magnitude(fe_ccircle_spatial_candidate$trees)
  }

  # Actually no candidate anymore if it made it that far
  fe_ccircle_spatial_candidate
}
