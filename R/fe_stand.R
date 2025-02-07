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




#' Constructor for the **fe_stand** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_stand}} for creating an
#' object of that class.
#'
#' @param x An appropriate \code{list} object
#'
#' @param ... Additional arguments required for enabling subclasses of
#'   \code{fe_stand}
#'
#' @param class A Character string required for enabling subclasses of
#'   \code{fe_stand}
#'
#' @return An object of class \code{fe_stand}
#'
#' @export
#'
#' @examples
#' # Constructing a minimal fe_stand object from scratch
#' # Use fe_stand() if you are not absolutely sure
#'
#' trees <- data.frame(
#'   tree_id = as.character(c(1:100)),
#'   species_id = as_fe_species_tum_wwk_short(rep("5", 100)),
#'   layer_key = 1,
#'   time_yr = 2022,
#'   dbh_cm = rnorm(100, 50, 8),
#'   age_yr = NA_real_,
#'   height_m = NA_real_,
#'   crown_base_height_m = NA_real_,
#'   crown_radius_m = NA_real_,
#'   removal = FALSE,
#'   ingrowth = FALSE,
#'   n_rep_ha = 1 / 0.75
#' )
#'
#' fe_stand_candidate <- list(
#'   stand_id = "my_interesting_stand",
#'   area_ha = 0.75,
#'   small_trees = data.frame(),
#'   trees = trees
#' )
#'
#' fe_stand_object <- new_fe_stand(fe_stand_candidate)
#'
#' # Better validate it
#' fe_stand_object |> validate_fe_stand()
#'
new_fe_stand <- function(x = list(), ..., class = character()) {
  stopifnot(is.list(x))

  structure(
    x,
    ...,
    class = c(class, "fe_stand")
  )
}




#' Check if an Object is an **fe_stand**
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_stand} class
#'
#' @export
#'
#' @examples
#' strange <- "xyzabc"
#' is_fe_stand(strange)
#'
#' is_fe_stand(norway_spruce_1_fe_stand)
#'
is_fe_stand <- function(x) {
  inherits(x, "fe_stand")
}




#' Validate an **fe_stand** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{new_fe_stand}}. Regular
#' users, please construct \code{fe_stand} objects with \code{\link{fe_stand}}.
#'
#' @param x An object that is expected to be a correct \code{fe_stand} object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the \code{fe_stand} object
#'   specifications. In case of such violations, the function will terminate
#'   with an error.
#'
#' @export
#'
#' @examples
#' # Validate the example stands
#' validate_fe_stand(norway_spruce_1_fe_stand)
#' validate_fe_stand(european_beech_1_fe_stand)
#' validate_fe_stand(spruce_beech_1_fe_stand)
#' selection_forest_1_fe_stand |> validate_fe_stand()
#'
validate_fe_stand <- function(x) {
  # Validation proceeds from the overall to the detail level

  # Check for class attribute
  stopifnot(is_fe_stand(x))


  # Check for presence of the required list elements
  required_names <- c("trees", "stand_id", "area_ha", "small_trees")
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
    ~name,         ~required,
    "trees",       "data.frame",
    "stand_id",    "character",
    "small_trees", "data.frame"
  )

  check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("element(s)", check_rslt$err_message),
      call. = FALSE
    )
  }


  # area_ha is an exception, because NULL is allowed, but if not NULL, it must
  # be numeric, must be checked separately
  if (!is.null(x$area_ha)) {
    required_types_or_classes <- tibble::tribble(
      ~name,         ~required,
      "area_ha",     "numeric",
    )

    check_rslt <- has_required_types_or_classes(x, required_types_or_classes)

    if (!check_rslt$rqmt_ok) {
      stop(
        paste("element(s)", check_rslt$err_message),
        call. = FALSE
      )
    }
  }

  # Check for length requirements for list elements with such restrictions
  required_lengths <- tibble::tribble(
    ~name,      ~required,
    "stand_id", 1L,
  )

  check_rslt <- has_required_lengths(x, required_lengths)

  if (!check_rslt$rqmt_ok) {
    stop(check_rslt$err_message, call. = FALSE)
  }


  # for the same reason as above, length of area_ha needs to be checked
  # separately
  if (!is.null(x$area_ha)) {
    required_lengths <- tibble::tribble(
      ~name,      ~required,
      "area_ha",  1L
    )

    check_rslt <- has_required_lengths(x, required_lengths)

    if (!check_rslt$rqmt_ok) {
      stop(check_rslt$err_message, call. = FALSE)
    }
  }


  # Check for presence of the required columns in data.frame trees
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


  # Check for required column types (empty columns (if allowed) must have the
  # correct NA type), species_id is not included here, will be checked below
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

  # Check species_id
  # - Coding supported?
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

  # - All codes ok?
  check_rslt <- all_codes_allowed(x$trees$species_id, spcs_coding)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("data.frame `trees`:", check_rslt$err_message),
      call. = FALSE
    )
  }

  # Check for missing values in columns which must be complete
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


  # Check for columns that must be distinct or distinct in combination
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

  # Return the object indepently from validation result
  x
}




#' User Friendly Construction of an **fe_stand** Object from a Data Frame
#'
#' \code{fe_stand()} provides a user-friendly interface for the constructor
#' \code{\link{new_fe_stand}}. While the constructor does not prevent users from
#' creating malformed \code{fe_stand} objects, \code{fe_stand} does everything
#' to achieve a well-defined object mostly based on an initial \code{data.frame}
#' that might be, e.g. drawn out of a user's own data base.
#'
#' The initial \code{data.frame} (or even nicer, \code{tibble}) provided by the
#' user must contain a a minimum set of columns (tree id, species id, time
#' variable, diameter at breast height). These columns must not contain missing
#' values. Other columns (containing tree height, height to crown base, crown
#' radius, tree age) are optional for the user to provide. If provided, they may
#' contain missing values. If not provided these columns will only contain
#' missing values in the \code{fe_stand} object. The columns about the trees'
#' removal and ingrowth status are also optional, but if provided, they must
#' *not* contain missing values. If not provided, both columns will be filled
#' with \code{FALSE} in the resulting \code{fe_stand} object.
#'
#' The columns from the user's \code{data.frame} that correspond to the columns
#' defined in \code{fe_stand} objects will turn up in the object under standard
#' names. All other columns that might be in the \code{data.frame} will be
#' transferred to the \code{fe_stand} object with their orignal names. It is the
#' user's responsibility to take care of them.
#'
#' \code{fe_stand} will automatically add a column \code{n_rep_ha} which
#' contains for each tree the number of trees it represents per ha. This may
#' seem redundant if looking at \code{fe_stand} objects alone, but it allows a
#' broad range of evaluation functions to be applied to different objects
#' containing trees.
#'
#'
#' @param x \code{data.frame} to be coerced into the goal object. Each line of
#'   \code{x} must represent a single tree. As a minimum requirement, there must
#'   be a column for the tree id, the species id, the time (in years), the
#'   diameter at breast height (dbh), each.
#'
#' @param tree_id_col name of the column in \code{x} which contains the tree
#'   id's (\code{character}, required, must not contain missing values)
#'
#' @param species_id_col  name of the column in \code{x} which contains the
#'   species id's. Must be an object of one  of the \code{fe_species} classes
#'   supported by this package. This column is required, must not contain
#'   missing values.
#'
#' @param time_yr_col name of the column in \code{x} which provides time
#'   information in years (\code{character}, required, must not contain missing
#'   values)
#'
#' @param dbh_cm_col name of the column in \code{x} which contains the dbh in cm
#'   (\code{character}, required, must not contain missing values)
#'
#' @param area_ha size of the stand area in ha (\code{numeric}. It is possible
#'   to have no defined stand area; in this case area_ha must be
#'   \code{NULL}, and n_rep_ha_col must be given (with no missing values).
#'
#' @param stand_id arbitrary id of the stand (\code{character}, default:
#'   "my_fe_stand")
#'
#' @param layer_key_col name of the column in \code{x} that contains codes for
#'   the stand layer a given tree belongs to. These codes are whole numbers.
#'   The following values are allowed: 1 - Main stand, 2 - Understorey,
#'   3 - Pregeneration (ger: "Vorausverjuengung"), 4 - Remnant trees, hold over
#'   trees, veteran trees (ger: "Nachhiebsreste", "Ueberhaelter", "Altbaeume").
#'   Must not contain missing values if provided. If not provided, it will be
#'   set to 1 (main stand) for every tree.
#'
#' @param age_yr_col name of the column in \code{x} which provides the tree ages
#'   in years (\code{character}, optional, may contain missing values)
#'
#' @param height_m_col name of the column in \code{x} which provides the tree
#'   heights in m (\code{character}, optional, may contain missing values)
#'
#' @param crown_base_height_m_col name of the column in \code{x} which provides
#'   the crown base heights in m (\code{character}, optional, may contain
#'   missing values)
#'
#' @param crown_radius_m_col name of the column in \code{x} which provides the
#'   crown radii in m (\code{character}, optional, may contain missing values)
#'
#' @param removal_col name of the column in \code{x} which provides the tree's
#'   removal status. If \code{TRUE}, this indicates the tree was removed or dead
#'   at the time indicated in \code{age_yr_col} (\code{character}, optional,
#'   must not contain missing values if provided). If not provided, the removal
#'   status will be \code{FALSE} for all trees in the resulting \code{fe_stand}
#'   object.
#'
#' @param ingrowth_col name of the column in \code{x} which provides the tree's
#'   ingrowth status. If \code{TRUE}, this indicates a tree that grew newly in
#'   at the time indicated in \code{age_yr_col} (\code{character}, optional,
#'   must not contain missing values if provided). If not provided, the ingrowth
#'   status will be \code{FALSE} for all trees in the resulting \code{fe_stand}
#'   object.
#'
#' @param n_rep_ha_col name of the column in the trees data frame which provides
#'   each tree's representation number per ha. Not required if a stand area is
#'   provided under \code{area_ha}. If a stand outline is given, n_rep_ha will
#'   be always recalculated based on the outline and the tree positions.
#'
#' @param small_trees An \code{fe_stand} object does contain an extra slot for
#'   small trees, defined as trees which are too small to have an own dbh (i.e.
#'   having a height > 1.3 m). So far, this slot is still experimental. The only
#'   requirement is that it is a \code{data.frame}. Such a \code{data.frame} can
#'   be provided via this parameter, it will be directly put into the goal
#'   object's \code{small_trees} slot. The default is a \code{data.frame} with
#'   zero rows and zero columns (\code{data.frame()}).
#'
#' @param verbose \code{logical}, if \code{TRUE} (default) the tree size
#'   variables will be checked for plausible orders of magnitude after
#'   successful construction of the \code{fe_stand} object. In case of a
#'   potential implausibility, a warning will be raised. The purpose of this
#'   mechanism is to avoid unit mismatches.
#'
#' @return If the user input allows to construct a well-defined \code{fe_stand}
#'   object, this object will be returned. If not, the function will terminate
#'   with an error.
#'
#' @export
#'
#' @examples
#' # Constructing an fe_stand object based on the minimum required information
#' # - make data.frame (or, nicer, a tibble) with stand information from
#' #   scratch
#' candidate_stand <- tibble::tibble(
#'   tree_no    = as.character(c(1:100)),
#'   species_id = as_fe_species_tum_wwk_short(c(rep("1", 30), rep("5", 70))),
#'   time_yr    = 2022,
#'   dbh        = c(rnorm(30, 45, 12), rnorm(70, 38, 9))
#' )
#'
#' # - call fe_stand
#' goal_fe_stand_object <- fe_stand(
#'   x = candidate_stand,
#'   tree_id_col = "tree_no",
#'   species_id_col = "species_id",
#'   time_yr_col = "time_yr",
#'   dbh_cm_col = "dbh",
#'   area_ha = 0.33
#' )
#'
#'
#' # Using raw data that could come out of a user's data bases; here one
#' # example stands (spruce_beech_1_raw) provided with the ForestElementsR
#' # package
#' spruce_beech_1_raw$year <- 2022 # No time information in the data frame
#' spruce_beech_1_raw$species <- as_fe_species_tum_wwk_short(
#'   spruce_beech_1_raw$species
#' )
#'
#' spruce_beech_stand <- fe_stand(
#'   spruce_beech_1_raw,
#'   tree_id_col = "no",
#'   species_id_col = "species",
#'   time_yr_col = "year",
#'   dbh_cm_col = "d",
#'   area_ha = 0.49,
#'   stand_id = spruce_beech_1_raw[1, ]$stand,
#'   age_yr_col = "age",
#'   height_m_col = "h",
#'   crown_base_height_m_col = "hcb",
#'   crown_radius_m_col = "crad"
#' )
#'
#' # Little summary
#' spruce_beech_stand |> summary()
#'
fe_stand <- function(x,
                     tree_id_col,
                     species_id_col,
                     time_yr_col,
                     dbh_cm_col,
                     area_ha,
                     stand_id = "my_fe_stand",
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

  stopifnot(is.data.frame(x))

  # <--- Start check for missing required arguments
  required_args <- c(
    "tree_id_col", "species_id_col", "time_yr_col", "dbh_cm_col", "area_ha"
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

  # Make the core data.frame trees of an fe_stand object
  # Remember: After assigning the class (below), the column n_rep_ha will be
  # possibly updated (if area_ha is given for the object)
  trees <- make_trees_dataframe(
    x,
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

  # Combine all ingredients into a list
  # No checks required here, because this will be covered by the validator
  # which is called below after the constructor
  fe_stand_candidate <- list(
    stand_id    = trimws(stand_id, which = "both"),
    area_ha     = area_ha,
    trees       = trees,
    small_trees = small_trees
  )

  # Most important: call the constructor
  fe_stand_candidate <- new_fe_stand(
    fe_stand_candidate
  )


  # if area_ha is given, n_rep_ha is calculated. In case, n_rep_ha is not
  # empty before, a warning will be issued
  if (!is.null(fe_stand_candidate$area_ha)) {
    if (any(!is.na(fe_stand_candidate$trees$n_rep_ha))) {
      warning(
        "recalculating existing n_rep_ha based on given stand area",
        call. = FALSE
      )
    }

    # (re-) calculate n_rep_ha in the data.frame trees (done here, because the
    # function n_rep_ha is an S3 generic)
    fe_stand_candidate$trees <- fe_stand_candidate$trees |>
      dplyr::mutate(n_rep_ha = n_rep_ha(fe_stand_candidate))
  }

  # Validate the object
  fe_stand_candidate <- validate_fe_stand(fe_stand_candidate)


  # Optional checks for plausible orders of magnitude
  if (verbose) check_trees_orders_of_magnitude(fe_stand_candidate$trees)

  # Actually no candidate anymore if it made it that far
  fe_stand_candidate
}
