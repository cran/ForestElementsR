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




# Functions intended for internal use in user friendly construction
# of fe_stand objects



#' Check the Orders of Magnitude of the Variables Given in a 'trees' data.frame
#' as Required in an *fe_stand* object
#'
#' @param trees data.frame (tibble) structured as required for the 'trees'
#'   component in an *fe_stand* object
#'
#' @return The function issues a warning if it suspects that one of the
#'   variables in \code{trees} does not have the right units. This is the side
#'   effect it is actually called for. It always returns \code{TRUE} invisibly.
#'
#' @keywords internal
#'
check_trees_orders_of_magnitude <- function(trees) {
  magn_ok <- check_order_of_magnitude(
    mean(trees$dbh_cm),
    min_ok = 0L, max_ok = 1L
  )
  if (!magn_ok) {
    warning(
      "unusual order of magnitude for `dbh_cm`. Are you sure it is in cm?",
      call. = FALSE
    )
  }
  if (!all(is.na(trees$height_m))) {
    magn_ok <- check_order_of_magnitude(
      mean(trees$height_m, na.rm = TRUE),
      min_ok = 0L, max_ok = 1L
    )
    if (!magn_ok) {
      warning(
        "unusual order of magnitude for `height_m`. Are you sure it is in m?",
        call. = FALSE
      )
    }
  }
  if (!all(is.na(trees$crown_base_height_m))) {
    magn_ok <- check_order_of_magnitude(
      mean(trees$crown_base_height_m, na.rm = TRUE),
      min_ok = 0L, max_ok = 1L
    )
    if (!magn_ok) {
      warning(
        paste(
          "unusual order of magnitude for `crown_base_height_m`.",
          "Are you sure it is in m?"
        ),
        call. = FALSE
      )
    }
  }
  if (!all(is.na(trees$crown_radius_m))) {
    magn_ok <- check_order_of_magnitude(
      mean(trees$crown_radius_m, na.rm = TRUE),
      min_ok = 0L, max_ok = 0L
    )
    if (!magn_ok) {
      warning(
        paste(
          "unusual order of magnitude for `crown_radius_m`.",
          "Are you sure it is in m?"
        ),
        call. = FALSE
      )
    }
  }
  if (!all(is.na(trees$age_yr))) {
    magn_ok <- check_order_of_magnitude(
      mean(trees$age_yr, na.rm = TRUE),
      min_ok = 0L, max_ok = 2L
    )
    if (!magn_ok) {
      warning(
        paste(
          "unusual order of magnitude for `age_yr`.",
          "Are you sure it is in years?"
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE) # Just to have a controled return value
}




#' Build a 'trees' data.frame (tibble) as Required for *fe_stand* and
#' *fe_stand_spatial* Objects
#'
#' @param x Input data frame (tibble)
#' @param tree_id_col See documentation of \code{\link{fe_stand}}
#' @param species_id_col See documentation of \code{\link{fe_stand}}
#' @param time_yr_col See documentation of \code{\link{fe_stand}}
#' @param dbh_cm_col See documentation of \code{\link{fe_stand}}
#' @param layer_key_col See documentation of \code{\link{fe_stand}}
#' @param age_yr_col See documentation of \code{\link{fe_stand}}
#' @param height_m_col See documentation of \code{\link{fe_stand}}
#' @param crown_base_height_m_col See documentation of \code{\link{fe_stand}}
#' @param crown_radius_m_col See documentation of \code{\link{fe_stand}}
#' @param removal_col See documentation of \code{\link{fe_stand}}
#' @param ingrowth_col See documentation of \code{\link{fe_stand}}
#' @param n_rep_ha_col See documentation of \code{\link{fe_stand}} and
#'   \code{\link{fe_stand_spatial}}
#'
#' @return A data.frame (tibble) that can be used as the list element 'trees' in
#'   an *fe_stand* or *fe_stand_spatial* object
#'
#' @keywords internal
#'
make_trees_dataframe <- function(x,
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
                                 n_rep_ha_col) {

  # First, make a data frame where the required columns which must be given
  # by the user have their correct names, but all columns supplied by the user
  # are included in addition
  trees <- x %>%
    tibble::as_tibble() %>%
    dplyr::select(
      tree_id       = tidyselect::all_of(tree_id_col),
      species_id    = tidyselect::all_of(species_id_col),
      time_yr       = tidyselect::all_of(time_yr_col),
      dbh_cm        = tidyselect::all_of(dbh_cm_col),
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      tree_id = trimws(as.character(.data$tree_id), which = "both")
    )


  # Second, take care for the optional columns (i.e. required columns, but
  # have to be supplied only optionally by the user)
  opt_cols <- tibble::tribble(
    ~required_name,          ~user_or_default_value,    ~required_type,
    #------------------------------------------------------------------
    "layer_key",             layer_key_col,             "numeric",
    "age_yr",                age_yr_col,                "double",
    "height_m",              height_m_col,              "double",
    "crown_base_height_m",   crown_base_height_m_col,   "double",
    "crown_radius_m",        crown_radius_m_col,        "double",
    "removal",               removal_col,               "logical",
    "ingrowth",              ingrowth_col,              "logical",
    "n_rep_ha",              n_rep_ha_col,              "double"
  )

  # Check if optional columns exist with their required names in the data, but
  # are not specified. Accept, but warn in this case and update opt_cols
  user_non_spec_cols <-
    opt_cols[is.na(opt_cols$user_or_default_value), ]$required_name

  if (length(user_non_spec_cols)) {
    better_not_there <-
      user_non_spec_cols[is.element(user_non_spec_cols, names(x))]

    if (length(better_not_there)) {
      # Accept such columns by defining them as user_or_default_value
      # in opt_cols
      opt_cols <- opt_cols |>
        dplyr::mutate(
          user_or_default_value =
            ifelse(.data$required_name %in% better_not_there,
              .data$required_name,
              .data$user_or_default_value
            )
        )

      # Issue a warning
      warning(
        paste(
          "optional column(s)", paste(better_not_there, collapse = ", "),
          "present in data, but not specified by user"
        ),
        call. = FALSE
      )
    }
  }


  # get user names of the user supplied optional columns (i.e. no NA), check
  # and rename as required if there area any - stop if the user specified names
  # which are not present in the provided data frame
  user_cols <-
    opt_cols[!is.na(opt_cols$user_or_default_value), ]$user_or_default_value

  if (length(user_cols)) {
    bad_user_names <- setdiff(user_cols, names(x))

    if (length(bad_user_names)) {
      stop(
        paste(
          "column(s)", paste(bad_user_names, collapse = ", "),
          "not existing in given data"
        )
      )
    }

    user_col_names_req <-
      opt_cols[!is.na(opt_cols$user_or_default_value), ]$required_name

    trees <- trees %>%
      dplyr::rename_with(~user_col_names_req,
        .cols = tidyselect::all_of(user_cols)
      )
  }


  # Fill the other optional columns with the defaults (NA for most, FALSE in
  # some cases)
  non_user_cols <-
    opt_cols[is.na(opt_cols$user_or_default_value), ]$required_name

  if (length(non_user_cols)) {
    # check if any double-type columns are not user-provided
    opt_dbl_cols <- opt_cols |>
      dplyr::filter(.data$required_type == "double") |>
      purrr::pluck("required_name")

    non_user_cols_dbl <- non_user_cols[non_user_cols %in% opt_dbl_cols]

    if (length(non_user_cols_dbl)) {
      m <- length(non_user_cols_dbl)
      interim_tib <- matrix(rep(NA_real_, m * nrow(trees)),
        ncol = length(non_user_cols_dbl)
      )
      colnames(interim_tib) <- non_user_cols_dbl
      interim_tib <- tibble::as_tibble(interim_tib)

      trees <- trees %>% dplyr::bind_cols(interim_tib)
    }

    # Any of removal, ingrowth (logical-type columns) not user-provided?
    opt_ri_cols <- opt_cols |>
      dplyr::filter(.data$required_type == "logical") |>
      purrr::pluck("required_name")

    non_user_cols_ri <- opt_ri_cols[opt_ri_cols %in% non_user_cols]

    if (length(non_user_cols_ri)) {
      m <- length(non_user_cols_ri)
      interim_tib <- matrix(rep(FALSE, m * nrow(trees)),
        ncol = length(non_user_cols_ri)
      )
      colnames(interim_tib) <- non_user_cols_ri
      interim_tib <- tibble::as_tibble(interim_tib)

      trees <- trees %>% dplyr::bind_cols(interim_tib)
    }

    # layer_key not user provided?
    if ("layer_key" %in% non_user_cols) {
      trees <- trees %>%
        dplyr::mutate(layer_key = 1)
    }
  }

  # arrange the columns nicely
  trees %>%
    dplyr::relocate(
      c(
        "tree_id", "species_id", "layer_key", "time_yr", "age_yr", "dbh_cm",
        "height_m", "crown_base_height_m", "crown_radius_m", "removal",
        "ingrowth", "n_rep_ha",
        dplyr::everything()
      )
    )
}
