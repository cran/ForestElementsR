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




#' Constructor for the **fe_stand_spatial** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_stand}} for creating an
#' object of that class.
#'
#' @param x An appropriate \code{list} object
#'
#' @param ... Additional arguments required for enabling subclasses of
#'   \code{fe_stand_spatial}
#'
#' @param class A Character string required for enabling subclasses of
#'   \code{fe_stand_spatial}
#'
#' @return An object of class \code{fe_stand_spatial}
#'
#' @export
#'
#' @examples
#' #' # Constructing a minimal fe_stand_spatial object from scratch
#' # Use fe_stand_spatial() if you are not absolutely sure
#'
#' trees <- data.frame(
#'   tree_id = as.character(c(1:50)),
#'   species_id = as_fe_species_tum_wwk_short(rep("5", 50)),
#'   layer_key = 1,
#'   time_yr = 2024,
#'   dbh_cm = rnorm(50, 50, 8),
#'   age_yr = NA_real_,
#'   height_m = NA_real_,
#'   crown_base_height_m = NA_real_,
#'   crown_radius_m = NA_real_,
#'   removal = FALSE,
#'   ingrowth = FALSE,
#'   n_rep_ha = 1 / 0.75
#' )
#'
#' # generate tree positions
#'
#' tree_positions <- data.frame(
#'  tree_id = as.character(c(1:50)),
#'  x_pos = runif(50, 1, 49),
#'  y_pos = runif(50, 1, 49)
#'  ) |> sf::st_as_sf(coords = c("x_pos", "y_pos"))
#'
#'
#' fe_stand_spatial_candidate <- list(
#'   stand_id = "my_interesting_stand",
#'   outline = NULL,
#'   orientation = 0,
#'   small_trees = data.frame(),
#'   trees = trees,
#'   tree_positions = tree_positions
#' )
#'
#' fe_stand_object <- new_fe_stand_spatial(fe_stand_spatial_candidate)
#'
#' # Better validate it
#' fe_stand_object |> validate_fe_stand_spatial()
#'
#'
#'
new_fe_stand_spatial <- function(x = list(), ..., class = character()) {

  stopifnot(is.list(x))
  parent_classes <- class(new_fe_stand(x))
  structure(
    x,
    ...,
    class = c(class, "fe_stand_spatial", parent_classes)
  )
}



#' Check if an Object is an **fe_stand_spatial**
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_stand_spatial}
#'   class
#'
#' @export
#'
#' @examples
#' strange <- "xyzabc"
#' is_fe_stand_spatial(strange)
#'
is_fe_stand_spatial <- function(x) {
  inherits(x, "fe_stand_spatial")
}



#' Validate an **fe_stand_spatial** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{new_fe_stand_spatial}}.
#' Regular users, please construct \code{fe_stand_spatial} objects with
#' \code{\link{fe_stand_spatial}}.
#'
#' @param x An object that is expected to be a correct \code{fe_stand_spatial}
#'   object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the \code{fe_stand_spatial}
#'   object specifications. In case of such violations, the function will
#'   terminate with an error.
#'
#' @export
#'
#' @examples
#'   # Validate the example fe_stand_spatial object
#'   mm_forest_1_fe_stand_spatial |> validate_fe_stand_spatial()
#'
validate_fe_stand_spatial <- function(x) {
  # obviously similar but not equal to the validation of an fe_stand object

  # Check for class attribute
  stopifnot(is_fe_stand_spatial(x))


  # Check for presence of the required list elements
  required_names <- c(
    "stand_id", "outline", "orientation", "trees", "tree_positions",
    "small_trees"
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
  # outline is checked separately, because it can by also NULL
  required_types_or_classes <- tibble::tribble(
    ~name, ~required,
    "trees", "data.frame",
    "tree_positions", "sf",
    "stand_id", "character",
    "orientation", "numeric",   # NA is better here, because means "unknown"
    "small_trees", "data.frame"
  )

  check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("element(s)", check_rslt$err_message),
      call. = FALSE
    )
  }

  # Type check for outline
  if (!is.null(x$outline)) {
    required_types_or_classes <- tibble::tribble(
      ~name, ~required,
      "outline", "POLYGON"
    )

    check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

    if (!check_rslt$rqmt_ok) {
      stop(
        paste("element(s)", check_rslt$err_message),
        call. = FALSE
      )
    }
  }

  # outline must not necessarily be given, but if it is (i.e. not NULL), it must
  # be not only just a polygon (see above), but a _valid_ polygon.
  if (!is.null(x$outline)) {
    # Check whether the stand outline is a valid polygon (i.e. no self-
    # intersections, etc.)
    check_rslt <- sf::st_is_valid(
      x$outline
    )

    # NA results for problems that would raise a GEOS error
    if (is.na(check_rslt) | !check_rslt) {
      stop(
        paste("given stand outline is not a valid polygon"),
        call. = FALSE
      )
    }
  }

  # Check for length requirements for list elements with such restrictions
  required_lengths <- tibble::tribble(
    ~name, ~required,
    "stand_id", 1L,
    "orientation", 1L,
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
    stop(
      "more tree_ids in `trees` than in `tree_positions`",
      call. = FALSE
    )
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


  # Return the object indepently from validation result
  x
}



#' User Friendly Construction of an **fe_stand_spatial** Object from a List of
#' Data Frames
#'
#' \code{fe_stand_spatial()} provides a user-friendly interface for the
#' constructor \code{\link{new_fe_stand_spatial}}. While the constructor does
#' not prevent users from creating malformed \code{fe_stand_spatial} objects,
#' \code{fe_stand_spatial} does everything to achieve a well-defined object
#' mostly based on an initial list of data.frames that might be, e.g. drawn out
#' of a user's own data base.
#'
#' An object of class \code{fe_stand_spatial} is a child object of
#' \code{fe_stand} which, however, contains information about the horizontal
#' positions of the trees and the horizontal outline of the stand. All spatial
#' information and its processing is based on the R-package
#' [sf](https://CRAN.R-project.org/package=sf).
#'
#' The input object \code{x} to \code{fe_stand_spatial} must be a list that
#' comprises two and an optional third data frame(s):
#' \itemize{ \item a data frame containing the single tree information (same
#' requirements as for \code{\link{fe_stand}}). This data frame must contain a a
#' minimum set of columns (tree id, species id, time variable, diameter at
#' breast height). These columns must not contain missing values. Other columns
#' (containing tree height, height to crown base, crown radius, tree age) are
#' optional for the user to provide. If provided, they may contain missing
#' values. If not provided, these columns will only contain missing values in
#' the \code{fe_stand_spatial} object. The columns about the trees' removal and
#' ingrowth status are also optional, but if provided, they must *not* contain
#' missing values. If not provided, both columns will be filled with
#' \code{FALSE} in the resulting \code{fe_stand_spatial} object.
#' \code{fe_stand_spatial} will automatically add a column \code{n_rep_ha} which
#' contains for each tree the number of trees it represents per ha. This may
#' seem redundant if looking at \code{fe_stand} objects alone, but it allows a
#' broad range of evaluation functions to be applied to different objects
#' containing trees. Note that in a \code{fe_stand_spatial} object trees are
#' allowed which are outside the actual stand outine (e.g. "buffer zone trees").
#' Such trees will be automatically identified by their coordinates and not
#' taken into account for calculating the stand's growth and yield
#' characteristics. However, they can be included in visual displays.
#'
#' \item a data frame that contains information about the tree positions. This
#' is not part of the first data frame, because the latter could contain several
#' observations (at different times) of the same tree, which would lead to
#' redundant coordinate representation. This data frame must contain a column
#' with tree id's, and the x and y coordinates of the stem center points
#' according to the same coordinate system as the stand outline (see below).
#' \code{NA} values are not allowed in this data frame.
#'
#' \item The optional third data frame must comprise the corner points of a
#' polygon that describes the stand's outline. The polygon must not be
#' self-intersection, therefore, the points must be given in correct sequence.
#' The first point does not need to be repeated - \code{fe_stand_spatial} will
#' take care for closing the polygon. This data frame needs to contain only the
#' x and y coordinates of the  corner points (in m), no \code{NA} values
#' allowed.}
#'
#' @param x named list of two or three data frames to be coerced into the goal
#'   object. One data frame must contain the single tree data; it has the same
#'   requirements as for the input data frame \code{x} of the function
#'   \code{\link{fe_stand}}. Another data frame must contain the tree positions.
#'   The third data frame is optional, if provided it must contain a coordinate
#'   description of the stand/plot outline. See the Details section for all
#'   requirements.
#'
#' @param tree_frame_name name of the data frame in \code{x} that contains the
#'   single tree data (default: "trees")
#'
#' @param tree_pos_frame_name name of the data frame in \code{x} that contains
#'   the tree positions (default: "tree_positions")
#'
#' @param outline_frame_name name of the data frame in \code{x} that contains
#'   the stand outline information (default: "outline"). It is possible to have
#'   no defined stand outline; in this case outline_frame_name must be
#'   \code{NULL}, and n_rep_ha_col must be given (with no missing values).
#'
#' @param orientation counterclockwise angle in degrees between the y-axis of
#'   the coordinate system of the stand outline and the tree positions and the
#'   north direction. The value \code{NA} is allowed.
#'
#' @param tree_id_col name of the column in the trees and tree_positions data
#'   frames  which contains the tree id's (\code{character}, required, must not
#'   contain missing values)
#'
#' @param species_id_col name of the column in trees data frame which contains
#'   the species id's. Must be an object of one  of the \code{fe_species}
#'   classes supported by this package. This column is required, must not
#'   contain missing values.
#'
#' @param time_yr_col name of the column in the trees data frame which provides
#'   time information in years (\code{character}, required, must not contain
#'   missing values)
#'
#' @param dbh_cm_col name of the column in the trees data frame which contains
#'   the dbh in cm (\code{character}, required, must not contain missing values)
#'
#' @param x_m_col name of the column in the tree positions data frame that
#'   contains the x-coordinates of the tree stem centers in m. The coordinates
#'   must adhere to the same coordinate system as the coordinates in the plot
#'   outline data frame. If the latter is provided, its x column must also have
#'   the name provided with \code{x_m_col}.
#'
#' @param y_m_col name of the column in the tree positions data frame that
#'   contains the y-coordinates of the tree stem centers in m. The coordinates
#'   must adhere to the same coordinate system as the coordinates in the plot
#'   outline data frame. If the latter is provided, its x column must also have
#'   the name provided with \code{y_m_col}.
#'
#' @param stand_id arbitrary id of the stand (\code{character}, default:
#'   "my_fe_stand_spatial")
#'
#' @param layer_key_col name of the column in \code{x} that contains codes for
#'   the stand layer a given tree belongs to. These codes are whole numbers.
#'   The following values are allowed: 1 - Main stand, 2 - Understorey,
#'   3 - Pregeneration (ger: "Vorausverjuengung"), 4 - Remnant trees, hold over
#'   trees, veteran trees (ger: "Nachhiebsreste", "Ueberhaelter", "Altbaeume").
#'   Must not contain missing values if provided. If not provided, it will be
#'   set to 1 (main stand) for every tree.
#'
#' @param age_yr_col name of the column in the trees data frame which provides
#'   the tree ages in years (\code{character}, optional, may contain missing
#'   values)
#'
#' @param height_m_col name of the column in the trees data frame which provides
#'   the tree heights in m (\code{character}, optional, may contain missing
#'   values)
#'
#' @param crown_base_height_m_col name of the column in the trees data frame
#'   which provides the crown base heights in m (\code{character}, optional, may
#'   contain missing values)
#'
#' @param crown_radius_m_col name of the column in the trees data frame which
#'   provides the crown radii in m (\code{character}, optional, may contain
#'   missing values)
#'
#' @param removal_col name of the column in the trees data frame which provides
#'   the tree's removal status. If \code{TRUE}, this indicates the tree was
#'   removed or dead at the time indicated in \code{age_yr_col}
#'   (\code{character}, optional, must not contain missing values if provided).
#'   If not provided, the removal status will be \code{FALSE} for all trees in
#'   the resulting \code{fe_stand} object.
#'
#' @param ingrowth_col name of the column in the trees data frame which provides
#'   the tree's ingrowth status. If \code{TRUE}, this indicates a tree that grew
#'   newly in at the time indicated in \code{age_yr_col} (\code{character},
#'   optional, must not contain missing values if provided). If not provided,
#'   the ingrowth status will be \code{FALSE} for all trees in the resulting
#'   \code{fe_stand} object.
#'
#' @param n_rep_ha_col name of the column in the trees data frame which provides
#'   each tree's representation number per ha. Not required if a stand outline
#'   is provided in \code{x}. If a stand outline is given, n_rep_ha will be
#'   always recalculated based on the outline and the tree positions.
#'
#' @param small_trees An \code{fe_stand_spatial} object does contain an extra
#'   slot for small trees, defined as trees which are too small to have an own
#'   dbh (i.e. having a height > 1.3 m). So far, this slot is still
#'   experimental. The only requirement is that it is a \code{data.frame}. Such
#'   a \code{data.frame} can be provided via this parameter, it will be directly
#'   put into the goal object's \code{small_trees} slot. The default is a
#'   \code{data.frame} with zero rows and zero columns (\code{data.frame()}).
#'
#' @param verbose name of the column in the trees data frame which provides the
#'   tree's ingrowth status. If \code{TRUE}, this indicates a tree that grew
#'   newly in at the time indicated in \code{age_yr_col} (\code{character},
#'   optional, must not contain missing values if provided). If not provided,
#'   the ingrowth status will be \code{FALSE} for all trees in the resulting
#'   \code{fe_stand} object.
#'
#' @return If the user input allows to construct a well-defined
#'   \code{fe_stand_spatial} object, this object will be returned. If not, the
#'   function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Transform the example data collection mm_forest_1_raw (could e.g. have
#' # been drawn out of a user's data base) into an fe_stand_spatial object
#'
#' mm_forest_spatial <- mm_forest_1_raw |>
#'   fe_stand_spatial(
#'     orientation = mm_forest_1_raw$north_dev,
#'     tree_id_col = "tree_id",
#'     species_id_col = "species_id",
#'     time_yr_col = "time_yr",
#'     dbh_cm_col = "dbh_cm",
#'     x_m_col = "x",
#'     y_m_col = "y",
#'     stand_id = mm_forest_1_raw$stand_id,
#'     height_m_col = "height_m",
#'     crown_base_height_m_col = "crown_base_height_m",
#'     crown_radius_m_col = "crown_radius_m",
#'     removal_col = "removal"
#'   )
#'
#' # Show a little summary, display scientific species names
#' options(fe_spec_lang = "sci")
#' mm_forest_spatial |> summary()
#'
fe_stand_spatial <- function(x,
                             tree_frame_name = "trees",
                             tree_pos_frame_name = "tree_positions",
                             outline_frame_name = "outline",
                             orientation = NA,
                             tree_id_col,
                             species_id_col,
                             time_yr_col,
                             dbh_cm_col,
                             x_m_col,
                             y_m_col,
                             stand_id = "my_fe_stand_spatial",
                             layer_key_col = NA,
                             age_yr_col = NA,
                             height_m_col = NA,
                             crown_base_height_m_col = NA,
                             crown_radius_m_col = NA,
                             removal_col = NA,
                             ingrowth_col = NA,
                             n_rep_ha_col = NA,
                             small_trees = data.frame(),
                             verbose = TRUE) {

  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(
    x[c(tree_frame_name, tree_pos_frame_name, outline_frame_name)],
    is.data.frame
  )))

  # <--- Start check for missing required arguments
  required_args <- c(
    "tree_id_col", "species_id_col", "time_yr_col", "dbh_cm_col", "x_m_col",
    "y_m_col"
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

  # Make the core data.frame trees of an fe_stand_spatial object
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

  # Make the tree_positions sfc, stop if required columns are missing
  tree_positions <- x[[tree_pos_frame_name]]

  check_rslt <-
    has_required_names(tree_positions, c(tree_id_col, x_m_col, y_m_col))

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
    dplyr::select(tidyselect::all_of(c(tree_id_col, x_m_col, y_m_col))) |>
    dplyr::rename(tree_id = tidyselect::all_of(tree_id_col)) |>
    dplyr::mutate(tree_id = as.character(.data$tree_id)) |>
    sf::st_as_sf(coords = c(x_m_col, y_m_col))


  # If the outline is provided
  # Transform the outline into an sf polygon, stop if required columns
  # are missing
  if (!is.null(outline_frame_name)) {
    outline <- x[[outline_frame_name]]

    check_rslt <-
      has_required_names(outline, c(x_m_col, y_m_col))

    if (!check_rslt$rqmt_ok) {
      stop(
        paste(
          "column(s)", check_rslt$err_message,
          "missing in `outline` data.frame"
        ),
        call. = FALSE
      )
    }

    outline <- outline |>
      dplyr::select(tidyselect::all_of(c(x_m_col, y_m_col))) |>
      dplyr::bind_rows(
        dplyr::slice(outline, 1L)
      ) |>
      as.matrix()

    outline <- sf::st_polygon(list(outline))
  } else {
    outline <- NULL
  }


  # Most important: call the constructor
  fe_stand_spatial_candidate <- new_fe_stand_spatial(
    list(
      stand_id       = trimws(stand_id, which = "both"),
      orientation    = orientation,
      outline        = outline,
      tree_positions = tree_positions,
      trees          = trees,
      small_trees    = small_trees
    )
  )

  # In case orientation is missing (i.e, NA) make it NA_real_, because
  # validation tests it for numeric
  if (is.na(fe_stand_spatial_candidate$orientation)) {
    fe_stand_spatial_candidate$orientation <- NA_real_
  }


  # if there is an outline, n_rep_ha is calculated. In case, n_rep_ha is not
  # empty before, a warning will be issued
  if (!is.null(fe_stand_spatial_candidate$outline)) {
    if (any(!is.na(fe_stand_spatial_candidate$trees$n_rep_ha))) {
      warning(
        "recalculating existing n_rep_ha based on outline and tree postitions",
        call. = FALSE
      )
    }

    # (re-) calculate n_rep_ha in the data.frame trees (done here, because the
    # function n_rep_ha is an S3 generic)
    fe_stand_spatial_candidate$trees <- fe_stand_spatial_candidate$trees |>
      dplyr::mutate(n_rep_ha = n_rep_ha(fe_stand_spatial_candidate))
  }

  # Validate the object
  fe_stand_spatial_candidate <-
    validate_fe_stand_spatial(fe_stand_spatial_candidate)


  # Optional checks for plausible orders of magnitude
  if (verbose) check_trees_orders_of_magnitude(fe_stand_spatial_candidate$trees)

  # Actually no candidate anymore if it made it that far
  fe_stand_spatial_candidate
}
