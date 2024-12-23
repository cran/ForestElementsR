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




# Functions intended for internal use in object validators
# intended for covering typical check situations




#' Check for Required Elements Existing in an Object
#'
#' @param x Any object for which \code{names} returns a character vector
#'
#' @param required_names Character vector of names required to be in
#'   \code{names(x)}
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements)
#'
#' @keywords internal
#'
has_required_names <- function(x, required_names) {
  actual_names <- names(x)
  i <- which(!(required_names %in% actual_names))
  err_message <- paste(
    paste(required_names[i], collapse = ", ")
  )

  list(
    rqmt_ok = length(i) == 0,
    err_message = err_message
  )
}




#' Check if an Object has a Required Type or Class
#'
#' Not a complete coverage of all base types and classes, but tailored to the
#' needs of \code{\link{validate_fe_stand}}
#'
#' @param x object to be checked
#'
#' @param rqmt name of the type or class required for \code{x}
#'   (\code{character})
#'
#' @return \code{TRUE} or \code{FALSE}, indicating if the requirement is met or
#'   not
#'
#' @keywords internal
#'
has_required_type_or_class <- function(x, rqmt) {
  # Remark: did not work as desired with dplyr::case_when
  switch(rqmt,
    "NULL"       = is.null(x),
    "NA"         = is.na(x),
    "list"       = inherits(x, "list"),
    "data.frame" = inherits(x, "data.frame"),
    "numeric"    = is.numeric(x),
    "double"     = is.double(x),
    "integer"    = is.integer(x),
    "logical"    = is.logical(x), # TRUE also for unspecified NA
    "character"  = is.character(x),
    "POLYGON"    = as.character(sf::st_geometry_type(x)) == "POLYGON",
    "sf"         = inherits(x, "sf"),
    "sfc"        = inherits(x, "sfc")
  )
}



#' Check Required Type or Class for a List of Objects
#'
#' @param x a list of objects to be checked
#'
#' @param names_and_rqmts a \code{tibble} that relates object names and the
#'   names of the types and classes they are required to have. Must have two
#'   \code{character} columns, \code{name} (i.e. the object name), and
#'   \code{required} (i.e. the name of the required type or class)
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is met and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements)
#'
#' @keywords internal
#'
has_required_types_or_classes <- function(x, names_and_rqmts) {
  stopifnot(tibble::is_tibble(names_and_rqmts))
  stopifnot(is.list(x))

  rslt <- purrr::map2_lgl(
    .x = x[names_and_rqmts$name], # check whether copied or just referenced
    .y = names_and_rqmts$required,
    .f = has_required_type_or_class
  )

  rqmt_ok <- all(rslt)
  err_message <- names_and_rqmts[!rslt, ] %>%
    dplyr::rowwise() %>%
    dplyr::summarise(
      paste(.data$name, "must be", .data$required)
    ) %>%
    purrr::pluck(1) %>%
    paste(collapse = ", ")

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message
  )
}




#' Check Required Length for a List of Objects
#'
#' @param x list of objects to be checked
#'
#' @param names_and_rqmts a \code{tibble} that relates object names and the
#'   length they are required to have. Must have two columns, \code{name} (i.e.
#'   the object name, \code{character}), and \code{required} (i.e. the required
#'   length, \code{integer})
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements)
#'
#' @keywords internal
#'
has_required_lengths <- function(x, names_and_rqmts) {
  stopifnot(tibble::is_tibble(names_and_rqmts))
  stopifnot(is.list(x))

  rslt <- purrr::map2_lgl(
    .x = x[names_and_rqmts$name],
    .y = names_and_rqmts$required,
    .f = ~ length(.x) == .y
  )

  rqmt_ok <- all(rslt)
  err_message <- names_and_rqmts[!rslt, ] %>%
    dplyr::rowwise() %>%
    dplyr::summarise(
      paste("length of", .data$name, "must be", .data$required)
    ) %>%
    purrr::pluck(1) %>%
    paste(collapse = ", ")

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message
  )
}





#' Check if Selected Columns in a Data Frame Contain Missing Values
#'
#' @param x a \code{data.frame}
#'
#' @param names \code{character} vector containg the names of the columns in
#'   \code{x} to be checked
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements)
#'
#' @keywords internal
#'

has_no_missing_values <- function(x, names) {
  stopifnot(is.data.frame(x))

  rslt <- purrr::map_lgl(
    .x = x[names],
    .f = ~ all(!is.na(.x))
  )

  rqmt_ok <- all(rslt)
  err_message <- paste(
    "column(s)",
    paste(names[!rslt], collapse = ", "),
    "must be complete"
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message
  )
}




#' Check if Given Variable Combinations in a Data Frame are Distinct
#'
#' @param x a \code{data.frame}
#'
#' @param combinations a \code{list} of \code{character} vectors, where each
#'   describes a variable combination in \code{x} to be checked for being
#'   distinct
#'
#' @return A list with two elements: 1) \code{rqmt_ok} which is \code{TRUE} if
#'   the requirement is fulfilled and \code{FALSE} otherwise; 2)
#'   \code{err_message}, the body of an error message to be used by the calling
#'   function (mentions all problematic elements)
#'
#' @keywords internal
#'
is_distinct <- function(x, combinations) {
  stopifnot(is.data.frame(x))
  stopifnot(is.list(combinations))

  x <- tibble::as_tibble(x) # Otherwise, the distinct(dat[, .x]) below will not
  # properly work for one-column .x

  rslt <- purrr::map_lgl(
    .x = combinations,
    .f = function(.x, dat) nrow(dplyr::distinct(dat[, .x])) == nrow(dat[, .x]),
    dat = x
  )

  rqmt_ok <- all(rslt)
  err_message <- paste(
    "column(s) / column combination(s)",
    paste(combinations[!rslt], collapse = ", "),
    "must be distinct"
  )

  list(
    rqmt_ok = rqmt_ok,
    err_message = err_message
  )
}



#' Check a Number's Order of Magnitude Against a Requirement
#'
#' @param x number to check
#'
#' @param min_ok an integer power of 10, allowed minimum
#'
#' @param max_ok an integer power of 10, allowed maximum
#'
#' @return TRUE if a number x's integer power of 10 is inside the range given by
#'   \code{min_ok} and \code{max_ok} (\code{min_ok} and \code{max_ok} included)
#'
#' @keywords internal
#'
check_order_of_magnitude <- function(x, min_ok, max_ok) {
  p <- as.integer(floor(log10(x)))
  (as.integer(min_ok) <= p) & (as.integer(p) <= max_ok)
}

















