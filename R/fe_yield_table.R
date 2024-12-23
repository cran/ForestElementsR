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




#' Constructor for the **fe_yield_table** Class
#'
#' Provided for expert users only who know exactly what they are doing. Other
#' users please take the function \code{\link{fe_yield_table}} for creating an
#' object of that class.
#'
#' @param x An appropriate \code{list} object
#'
#' @return An object of class \code{fe_yield_table}
#'
#' @export
#'
#' @examples
#'   # Some highly motivated object, even a list
#'   x <- list(my_dream = "I want to be a yield table!")
#'   x <- new_fe_yield_table(x) # let's help the guy
#'   is_fe_yield_table(x)       # nice - it worked?!
#'
#'   # But here's the error. That's why you should not use the bare constructor
#'   # if you don't know what you are doing.
#'   try(
#'     validate_fe_yield_table(x)
#'   )
#'
new_fe_yield_table <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "fe_yield_table")
}




#' Check if an Object is an **fe_yield_table**
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_yield_table}
#'   class
#'
#' @export
#'
#' @examples
#'   x <- "I want to be a yield table!"  # strange object
#'   is_fe_yield_table(x)                # sorry
#'
#'   is_fe_yield_table(ytable_pine_wiedemann_moderate_1943_raw) # Nope
#'   is_fe_yield_table(fe_ytable_pine_wiedemann_moderate_1943)  # That's better
#'
is_fe_yield_table <- function(x) {
  inherits(x, "fe_yield_table")
}




#' Validate a Candidate For an fe_yield_table Object
#'
#' @param x The candidate object to be validated
#'
#' @return If \code{x} is not a valid \code{fe_yield_table} object, the function
#'  will terminate with an error. Otherwise, \code{x} will be returned.
#'
#' @export
#'
#' @examples
#'  validate_fe_yield_table(fe_ytable_beech_wiedemann_moderate_1931)
#'  validate_fe_yield_table(fe_ytable_larch_schober_moderate_1946)
#'  validate_fe_yield_table(fe_ytable_pine_wiedemann_moderate_1943)
#'  validate_fe_yield_table(fe_ytable_spruce_gehrhardt_moderate_1921)
#'  validate_fe_yield_table(fe_ytable_douglas_schober_moderate_1956)
#'  validate_fe_yield_table(fe_ytable_spruce_wiedemann_moderate_1936_42)
#'  validate_fe_yield_table(fe_ytable_silver_fir_hausser_moderate_1956)
#'
validate_fe_yield_table <- function(x) {

  stopifnot(is_fe_yield_table(x))

  # Are the required names on top level present?
  check_rslt <- has_required_names(
    x,
    required_names = c(
      "name_orig", "name_international", "site_indexes", "site_index_variable",
      "mai_variable", "age_coverage", "values"
    )
  )
  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "Element(s)", check_rslt$err_message,
        "missing in fe_yield_table candidate."
      ),
      call. = FALSE
    )
  }


  if (nchar(x$name_orig) < 2) {
    stop(
      "No valid original yield table name.",
      call. = FALSE
    )
  }

  if (nchar(x$name_international) < 2) {
    stop(
      "No valid international yield table name.",
      call. = FALSE
    )
  }

  if (any(is.na(x$site_indexes))) {
    stop(
      "fe_yield_table element 'site_indexes' must not contain NA values.",
      call. = FALSE
    )
  }

  if (is.unsorted(x$site_indexes)) {
    stop(
      paste0(
        "fe_yield_table element 'site_indexes' must be sorted in increasing ",
        "order."),
      call. = FALSE
    )
  }

  if (any(duplicated(x$site_indexes))) {
    stop(
      "fe_yield_table element 'site_indexes' must be unique.",
      call. = FALSE
    )
  }


  # Are the required elements present (with their names) in the value slot?
  ## Generally required names
  check_rslt <- has_required_names(
    x$values,
    required_names = c(
      "ba_m2_ha", "v_m3_ha", "pai_m3_ha_yr", "mai_m3_ha_yr", "tvp_m3_ha"
    )
  )
  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "Element(s)", check_rslt$err_message,
        "missing in the value slot of the fe_yield_table candidate."
      ),
      call. = FALSE
    )
  }


  ## Site index variable name(s)
  check_rslt <- has_required_names(
    x$values,
    required_names = x$site_index_variable
  )
  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "Named site index variable(s)", check_rslt$err_message,
        "missing in the value slot of the fe_yield_table candidate."
      ),
      call. = FALSE
    )
  }


  ## Mai variable name(s), if applicable
  if (any(!is.na(x$mai_variable))) {
    check_rslt <- has_required_names(
      x$values,
      required_names = stats::na.omit(x$mai_variable)
    )
    if (!check_rslt$rqmt_ok) {
      stop(
        paste(
          "Named mai variable(s)", check_rslt$err_message,
          "missing in the value slot of the fe_yield_table candidate."
        ),
        call. = FALSE
      )
    }
  }


  if (any(is.na(x$age_coverage))) {
    stop(
      "fe_yield_table element 'age_coverage' must not contain NA values.",
      call. = FALSE
    )
  }

  if (is.unsorted(x$age_coverage)) {
    stop(
      paste0(
        "fe_yield_table element 'age_coverage' must be sorted in increasing ",
        "order."),
      call. = FALSE
    )
  }

  if (any(duplicated(x$age_coverage))) {
    stop(
      "fe_yield_table element 'age_coverage' must be unique.",
      call. = FALSE
    )
  }

  # Are all value slots matrices?
  if (any(!purrr::map_lgl(x$values, is.matrix))) {
    stop(
      "All value elements in an fe_yield_table object must be matrices.",
      call. = FALSE
    )
  }

  # Do all the matrices have the correct dimensions?
  # - columns
  if (
    any(
      purrr::map_lgl(
        x$values,
        .f = function(val_mat, si) ncol(val_mat) != length(si),
        si = x$site_indexes
      )
    )
  ) {
    stop(
      paste0(
        "The column number of all value matrices in an fe_yield_table object ",
        "must by equal to the length of the site_indexes vector."
      ),
      call. = FALSE
    )
  }

  # - rows
  if (
    any(
      purrr::map_lgl(
        x$values,
        .f = function(val_mat, agc) nrow(val_mat) != length(agc),
        agc = x$age_coverage
      )
    )
  ) {
    stop(
      paste0(
        "The row number of all value matrices in an fe_yield_table object ",
        "must by equal to the length of the age_coverage vector."
      ),
      call. = FALSE
    )
  }

  # All value matrices must be type double
  names_and_rqmts <- tibble::tibble(
    name      = names(x$values),
    required  = "double"
  )
  check_rslt <- has_required_types_or_classes(x$values, names_and_rqmts)
  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "Value matrix/matrices",
        check_rslt$err_message,
        "is/are not of type double."
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------------
  # TODO Add more evaluations of the actual values here
  # Do the numeric values keep minimum requirements?
  # -------------------------------------------------------------

  x
}




#' User Friendly Construction of an **fe_yield_table** Object from a Data Frame
#'
#' @param x Data frame to become an \code{fe_yield_table} object. The minimum
#'   required columns (all numeric) and their names are:
#'
#'   \itemize{
#'    \item{age: Stand age in years}
#'    \item{si: Site index following the yield table's definition}
#'    \item{ba_m2_ha: The stand's standing basal area in m²/ha}
#'    \item{v_m3_ha: The stand's standing volume in m³/ha}
#'    \item{pai_m3_ha_yr: The periodic annual volume increment in m³/ha/yr}
#'    \item{mai_m3_ha_yr: The mean annual volume increment in m³/ha/yr}
#'   }
#'
#'   The variable(s) named as the input parameters \code{si_variable} and
#'   \code{mai_variable} must be present in addition. NA values are allowed.
#'   All other columns of \code{x} will also be taken into the yield table
#'   object and will be accessible by all functions that request values from
#'   yield tables.
#'
#' @param name_orig Name of the table in the language it was originally
#'   published
#'
#' @param name_int Internatonalized (i.e. English) version of the table name
#'
#' @param si_variable Character (vector), name(s) of the variable(s) (i.e.
#'   column(s) in x) which is/are used for site indexing. Usually, this will be
#'   stand heights. Some yield tables contain different definitions of stand
#'   heights (e.g. mean and dominant height). In order to enable site indexing
#'   by more than one height definition in such a case, more than one variable
#'   name can be provided here. At least one site index variable must be given
#'   to obtain a valid fe_yield_table object.
#'
#' @param mai_variable Character (vector), name(s) of the variable(s) (i.e.
#'   column(s) in x) which can be used for mai-site indexing. Clearly, this will
#'   be variables containing a mean annual increment (mai). Some yield tables
#'   contain different definitions of mai (e.g. over- and under bark).
#'   Therefore, more than one variable name can be provided here. If no mai
#'   variable is provided (\code{mai_variable = NA}, default), mai site indexing
#'   is not possible with the yield table of interest.
#'
#' @return An object of class \code{fe_yield_table}, if the input allows to
#'   build a valid one. If this is not the case, the function will terminate
#'   with an error.
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   # Make fe_yield_table object from a very original-table-like data frame
#'   ytable_pine_wiedemann_moderate_1943_raw |>
#'     fe_yield_table(
#'       name_orig = "Kiefer Wiedemann 1943 Maessige Durchforstung",
#'       name_int  = "Scots Pine Wiedemann 1943 Moderate Thinning",
#'       si_variable = "h_q_m",
#'       mai_variable = c("mai_m3_ha_yr", "red_mai_m3_ha_yr")
#'     )
#'
fe_yield_table <- function(x,
                           name_orig,
                           name_int,
                           si_variable  = "h_q_m",
                           mai_variable = NA
                           ) {

  stopifnot(is.data.frame(x))

  # names of the variables to make matrices from later
  vars_ <- names(x)
  vars_ <- vars_[!(vars_ %in% c("age", "si"))]

  # site indexes covered by the table, ascending order independent from absolute
  # or relatrive site index definition (only the fact that they are ordered is
  # important)
  site_indexes <- sort(unique(x$si))

  # ages covered by the table
  age_coverage <- sort(unique(x$age))

  # sort data by age and site index to be on the safe side
  ytbl <- x |>
    dplyr::arrange(.data$si, .data$age) |>
    tidyr::pivot_longer(cols = !.data$age & !.data$si, names_to = "variable") |>
    tidyr::pivot_wider(names_from = .data$si, names_prefix = "si_")

  # make a named list of matrices for each variable. Each row is an age, each
  # column is a site index
  ytbl <- vars_ |> purrr::map(
    .f = function(.x, dat) {
      dat <- dat |>
        dplyr::filter(.data$variable == .x)
      age <- dat$age # to be used as rownames
      dat <- dat |>
        dplyr::select(dplyr::starts_with("si_")) |>
        as.matrix()
      rownames(dat) <- age
      dat
    },
    dat = ytbl
  )
  names(ytbl) <- vars_ # to identify the variable stored in each matrix

  ytbl = list(
    name_orig           = name_orig,
    name_international  = name_int,
    site_indexes        = site_indexes,
    site_index_variable = si_variable,
    mai_variable        = mai_variable,
    age_coverage        = age_coverage,
    values              = ytbl
  )

  ytbl <- new_fe_yield_table(ytbl)

  validate_fe_yield_table(ytbl)
}







