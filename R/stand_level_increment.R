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




#' stand_level_increment
#'
#' Calculate periodic annual stand level increments from time-ordered vectors of
#' appropriate stand sum variables. Typically the variable of interest is a
#' stand's wood volume per unit area, but it works equally for stand basal area
#' and biomass.
#'
#' The input vector \code{x_remove} is to be understood as the amount removed up
#' to (or at) the corresponding points in time and after the preceding points in
#' time in the \code{time} input vector. The resulting increments are to be
#' understood in a similar way: The entries in the increment vector relate to
#' the period between the entry at the same position in the \code{time} input
#' vector and the time entry at the position before. Therefore, the first
#' element of the resulting increment vector is always \code{NA}.
#'
#' @param time Vector of points in time. Must be unique and in ascending order
#'
#' @param x_remain Vector of the variable of interest (typically wood volume)
#'   for the remaining stand. "Remaining stand" means the amount which is
#'   actually there at the corresponding point in time. The vector
#'   \code{x_remaing} must be arranged corresponding to the input vector
#'   \code{time}.
#'
#' @param x_remove Vector of the variable of interest (typically wood volume)
#'   for the removal stand. Each entry in this vector represents the amount
#'   removed up to and including the corresponding point of time, but after the
#'   previous point in time.
#'
#' @return A vector of the annual increments for the stand level variable of
#'   interest corresponding to the input vector \code{time}. The increments
#'   always relate to the period from (and including) the corresponding point
#'   in time back to (and excluding) the previous point in time. Thus, the first
#'   element of the output vector is always NA. If the input vectors are of
#'   length 1 only, the function consequently returns NA.
#'
#' @export
#'
#' @examples
#'  # Stand age, remaining and removal volume (m³/ha)
#'  age        <- seq(20, 70, 5)
#'  vol_remain <- c(65, 118, 175, 233, 293, 355, 416, 476, 534, 589, 642)
#'  vol_remove <- c(16,  29,  35,  39,  39,  39,  38,  37,  36,  35,  34)
#'
#'  stand_level_increment(age, vol_remain, vol_remove) # m³/ha/yr
#'
#'  # Works also with basal area (m²/ha)
#'  age        <- seq(20, 60, 5)
#'  ba_remain  <- c(26.0, 30.1, 32.5, 34.2, 35.5, 37.1, 38.7, 40.3, 41.9)
#'  ba_remove  <- c( 0.0,  5.0,  5.9,  5.4,  5.1,  4.2,  3.3,  3.0,  2.7)
#'
#'  stand_level_increment(age, ba_remain, ba_remove) # m²/ha/yr
#'
stand_level_increment <- function(time, x_remain, x_remove) {

  # The three input vectors must have the same length
  all_same_lgth <- 1 == length(unique(lengths(list(x_remain, x_remove, time))))

  if (!all_same_lgth) {
    stop(
      "All input vectors to stand_level_increment() must have the same length"
    )
  }

  # No NAs allowed in any input vector
  any_na <- any(
    sapply(
      list(x_remain, x_remove, time), FUN = function(x) any(is.na(x))
    )
  )

  if (any_na) {
    stop(
      "No NA values allowed in any input vector"
    )
  }

  # Time vector must be unique (otherwise divisions by zero)
  if (length(time) != length(unique(time))) {
    stop(
      "No equal elements allowed in time vector"
    )
  }

  # Time vector must be in ascending order
  if (!identical(time, sort(time))) {
    stop(
      "Time vector must be in ascending order"
    )
  }

  # The other vectors are assumed to follow the time vector in their order,
  # and as we have tested the time vector before, we can proceed by calculating
  # the total production and the increment in the classic way
  x_total_production <- x_remain + cumsum(x_remove)
  # The increment is always entried at the end point of time of the period it
  # belongs to. First Element of x_increment is therefore NA
  x_increment <- x_total_production - dplyr::lag(x_total_production)
  # Make the increments annual
  per_length <- time - dplyr::lag(time)

  x_increment / per_length
}






